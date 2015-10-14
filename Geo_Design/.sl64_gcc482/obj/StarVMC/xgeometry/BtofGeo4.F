C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO4                                                 1
C BTOFGEO4 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO4'/, BANK_TITLE/'*'/                            1
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
      REAL BTOG_POSIT3                                                    11
      EQUIVALENCE (BANK_BTOG(11),BTOG_POSIT3,RE_BTOG_POSIT3),             11
     *(NAMES_BTOG(8),NBTOG_POSIT3)                                        11
      EQUIVALENCE (COMMEN_BTOG(8),CBTOG_POSIT3),(MAP_BTOG(1,8),           11
     *MBTOG_POSIT3)                                                       11
      CHARACTER CBTOG_POSIT3*40/' '/                                      11
      CHARACTER NBTOG_POSIT3*12/'R.POSIT3'/                               11
      INTEGER MBTOG_POSIT3(2)/1,1/                                        11
      REAL RE_BTOG_POSIT3                                                 11
C -                                                                       11
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                11
      PARAMETER (LEN_BTOG=11)                                             11
      PARAMETER (LLEN_BTOG=8)                                             11
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
C -                                                                       34
      REAL MOD5_HEIGHT                                                    34
      EQUIVALENCE (BANK_MOD5(3),MOD5_HEIGHT,RE_MOD5_HEIGHT),              34
     *(NAMES_MOD5(1),NMOD5_HEIGHT)                                        34
      EQUIVALENCE (COMMEN_MOD5(1),CMOD5_HEIGHT),(MAP_MOD5(1,1),           34
     *MMOD5_HEIGHT)                                                       34
      CHARACTER CMOD5_HEIGHT*40/' '/                                      34
      CHARACTER NMOD5_HEIGHT*12/'R.HEIGHT'/                               34
      INTEGER MMOD5_HEIGHT(2)/1,1/                                        34
      REAL RE_MOD5_HEIGHT                                                 34
C -                                                                       34
      REAL MOD5_WIDTH                                                     34
      EQUIVALENCE (BANK_MOD5(4),MOD5_WIDTH,RE_MOD5_WIDTH),                34
     *(NAMES_MOD5(2),NMOD5_WIDTH)                                         34
      EQUIVALENCE (COMMEN_MOD5(2),CMOD5_WIDTH),(MAP_MOD5(1,2),            34
     *MMOD5_WIDTH)                                                        34
      CHARACTER CMOD5_WIDTH*40/' '/                                       34
      CHARACTER NMOD5_WIDTH*12/'R.WIDTH'/                                 34
      INTEGER MMOD5_WIDTH(2)/1,1/                                         34
      REAL RE_MOD5_WIDTH                                                  34
C -                                                                       34
      REAL MOD5_LENGTH                                                    34
      EQUIVALENCE (BANK_MOD5(5),MOD5_LENGTH,RE_MOD5_LENGTH),              34
     *(NAMES_MOD5(3),NMOD5_LENGTH)                                        34
      EQUIVALENCE (COMMEN_MOD5(3),CMOD5_LENGTH),(MAP_MOD5(1,3),           34
     *MMOD5_LENGTH)                                                       34
      CHARACTER CMOD5_LENGTH*40/' '/                                      34
      CHARACTER NMOD5_LENGTH*12/'R.LENGTH'/                               34
      INTEGER MMOD5_LENGTH(2)/1,1/                                        34
      REAL RE_MOD5_LENGTH                                                 34
C -                                                                       34
      REAL MOD5_CENTER                                                    34
      EQUIVALENCE (BANK_MOD5(6),MOD5_CENTER,RE_MOD5_CENTER),              34
     *(NAMES_MOD5(4),NMOD5_CENTER)                                        34
      EQUIVALENCE (COMMEN_MOD5(4),CMOD5_CENTER),(MAP_MOD5(1,4),           34
     *MMOD5_CENTER)                                                       34
      CHARACTER CMOD5_CENTER*40/' '/                                      34
      CHARACTER NMOD5_CENTER*12/'R.CENTER'/                               34
      INTEGER MMOD5_CENTER(2)/1,1/                                        34
      REAL RE_MOD5_CENTER                                                 34
C -                                                                       34
      REAL MOD5_MRPCX(32)                                                 34
      EQUIVALENCE (BANK_MOD5(7),MOD5_MRPCX,RE_MOD5_MRPCX),                34
     *(NAMES_MOD5(5),NMOD5_MRPCX)                                         34
      EQUIVALENCE (COMMEN_MOD5(5),CMOD5_MRPCX),(MAP_MOD5(1,5),            34
     *MMOD5_MRPCX)                                                        34
      CHARACTER CMOD5_MRPCX*40/' '/                                       34
      CHARACTER NMOD5_MRPCX*12/'R.MRPCX'/                                 34
      INTEGER MMOD5_MRPCX(2)/32,1/                                        34
      REAL RE_MOD5_MRPCX                                                  34
C -                                                                       34
      REAL MOD5_MRPCZ(32)                                                 34
      EQUIVALENCE (BANK_MOD5(39),MOD5_MRPCZ,RE_MOD5_MRPCZ),               34
     *(NAMES_MOD5(6),NMOD5_MRPCZ)                                         34
      EQUIVALENCE (COMMEN_MOD5(6),CMOD5_MRPCZ),(MAP_MOD5(1,6),            34
     *MMOD5_MRPCZ)                                                        34
      CHARACTER CMOD5_MRPCZ*40/' '/                                       34
      CHARACTER NMOD5_MRPCZ*12/'R.MRPCZ'/                                 34
      INTEGER MMOD5_MRPCZ(2)/32,1/                                        34
      REAL RE_MOD5_MRPCZ                                                  34
C -                                                                       34
      REAL MOD5_MRPCA(32)                                                 34
      EQUIVALENCE (BANK_MOD5(71),MOD5_MRPCA,RE_MOD5_MRPCA),               34
     *(NAMES_MOD5(7),NMOD5_MRPCA)                                         34
      EQUIVALENCE (COMMEN_MOD5(7),CMOD5_MRPCA),(MAP_MOD5(1,7),            34
     *MMOD5_MRPCA)                                                        34
      CHARACTER CMOD5_MRPCA*40/' '/                                       34
      CHARACTER NMOD5_MRPCA*12/'R.MRPCA'/                                 34
      INTEGER MMOD5_MRPCA(2)/32,1/                                        34
      REAL RE_MOD5_MRPCA                                                  34
C -                                                                       34
      REAL MOD5_HCHGT                                                     34
      EQUIVALENCE (BANK_MOD5(103),MOD5_HCHGT,RE_MOD5_HCHGT),              34
     *(NAMES_MOD5(8),NMOD5_HCHGT)                                         34
      EQUIVALENCE (COMMEN_MOD5(8),CMOD5_HCHGT),(MAP_MOD5(1,8),            34
     *MMOD5_HCHGT)                                                        34
      CHARACTER CMOD5_HCHGT*40/' '/                                       34
      CHARACTER NMOD5_HCHGT*12/'R.HCHGT'/                                 34
      INTEGER MMOD5_HCHGT(2)/1,1/                                         34
      REAL RE_MOD5_HCHGT                                                  34
C -                                                                       34
      REAL MOD5_HCWID                                                     34
      EQUIVALENCE (BANK_MOD5(104),MOD5_HCWID,RE_MOD5_HCWID),              34
     *(NAMES_MOD5(9),NMOD5_HCWID)                                         34
      EQUIVALENCE (COMMEN_MOD5(9),CMOD5_HCWID),(MAP_MOD5(1,9),            34
     *MMOD5_HCWID)                                                        34
      CHARACTER CMOD5_HCWID*40/' '/                                       34
      CHARACTER NMOD5_HCWID*12/'R.HCWID'/                                 34
      INTEGER MMOD5_HCWID(2)/1,1/                                         34
      REAL RE_MOD5_HCWID                                                  34
C -                                                                       34
      REAL MOD5_HCLEN                                                     34
      EQUIVALENCE (BANK_MOD5(105),MOD5_HCLEN,RE_MOD5_HCLEN),              34
     *(NAMES_MOD5(10),NMOD5_HCLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(10),CMOD5_HCLEN),(MAP_MOD5(1,10),          34
     *MMOD5_HCLEN)                                                        34
      CHARACTER CMOD5_HCLEN*40/' '/                                       34
      CHARACTER NMOD5_HCLEN*12/'R.HCLEN'/                                 34
      INTEGER MMOD5_HCLEN(2)/1,1/                                         34
      REAL RE_MOD5_HCLEN                                                  34
C -                                                                       34
      REAL MOD5_PCBHGT                                                    34
      EQUIVALENCE (BANK_MOD5(106),MOD5_PCBHGT,RE_MOD5_PCBHGT),            34
     *(NAMES_MOD5(11),NMOD5_PCBHGT)                                       34
      EQUIVALENCE (COMMEN_MOD5(11),CMOD5_PCBHGT),(MAP_MOD5(1,11),         34
     *MMOD5_PCBHGT)                                                       34
      CHARACTER CMOD5_PCBHGT*40/' '/                                      34
      CHARACTER NMOD5_PCBHGT*12/'R.PCBHGT'/                               34
      INTEGER MMOD5_PCBHGT(2)/1,1/                                        34
      REAL RE_MOD5_PCBHGT                                                 34
C -                                                                       34
      REAL MOD5_PCBWID                                                    34
      EQUIVALENCE (BANK_MOD5(107),MOD5_PCBWID,RE_MOD5_PCBWID),            34
     *(NAMES_MOD5(12),NMOD5_PCBWID)                                       34
      EQUIVALENCE (COMMEN_MOD5(12),CMOD5_PCBWID),(MAP_MOD5(1,12),         34
     *MMOD5_PCBWID)                                                       34
      CHARACTER CMOD5_PCBWID*40/' '/                                      34
      CHARACTER NMOD5_PCBWID*12/'R.PCBWID'/                               34
      INTEGER MMOD5_PCBWID(2)/1,1/                                        34
      REAL RE_MOD5_PCBWID                                                 34
C -                                                                       34
      REAL MOD5_PCBLEN                                                    34
      EQUIVALENCE (BANK_MOD5(108),MOD5_PCBLEN,RE_MOD5_PCBLEN),            34
     *(NAMES_MOD5(13),NMOD5_PCBLEN)                                       34
      EQUIVALENCE (COMMEN_MOD5(13),CMOD5_PCBLEN),(MAP_MOD5(1,13),         34
     *MMOD5_PCBLEN)                                                       34
      CHARACTER CMOD5_PCBLEN*40/' '/                                      34
      CHARACTER NMOD5_PCBLEN*12/'R.PCBLEN'/                               34
      INTEGER MMOD5_PCBLEN(2)/1,1/                                        34
      REAL RE_MOD5_PCBLEN                                                 34
C -                                                                       34
      REAL MOD5_MYHGT                                                     34
      EQUIVALENCE (BANK_MOD5(109),MOD5_MYHGT,RE_MOD5_MYHGT),              34
     *(NAMES_MOD5(14),NMOD5_MYHGT)                                        34
      EQUIVALENCE (COMMEN_MOD5(14),CMOD5_MYHGT),(MAP_MOD5(1,14),          34
     *MMOD5_MYHGT)                                                        34
      CHARACTER CMOD5_MYHGT*40/' '/                                       34
      CHARACTER NMOD5_MYHGT*12/'R.MYHGT'/                                 34
      INTEGER MMOD5_MYHGT(2)/1,1/                                         34
      REAL RE_MOD5_MYHGT                                                  34
C -                                                                       34
      REAL MOD5_MYWID                                                     34
      EQUIVALENCE (BANK_MOD5(110),MOD5_MYWID,RE_MOD5_MYWID),              34
     *(NAMES_MOD5(15),NMOD5_MYWID)                                        34
      EQUIVALENCE (COMMEN_MOD5(15),CMOD5_MYWID),(MAP_MOD5(1,15),          34
     *MMOD5_MYWID)                                                        34
      CHARACTER CMOD5_MYWID*40/' '/                                       34
      CHARACTER NMOD5_MYWID*12/'R.MYWID'/                                 34
      INTEGER MMOD5_MYWID(2)/1,1/                                         34
      REAL RE_MOD5_MYWID                                                  34
C -                                                                       34
      REAL MOD5_MYLEN                                                     34
      EQUIVALENCE (BANK_MOD5(111),MOD5_MYLEN,RE_MOD5_MYLEN),              34
     *(NAMES_MOD5(16),NMOD5_MYLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(16),CMOD5_MYLEN),(MAP_MOD5(1,16),          34
     *MMOD5_MYLEN)                                                        34
      CHARACTER CMOD5_MYLEN*40/' '/                                       34
      CHARACTER NMOD5_MYLEN*12/'R.MYLEN'/                                 34
      INTEGER MMOD5_MYLEN(2)/1,1/                                         34
      REAL RE_MOD5_MYLEN                                                  34
C -                                                                       34
      REAL MOD5_GRHGT                                                     34
      EQUIVALENCE (BANK_MOD5(112),MOD5_GRHGT,RE_MOD5_GRHGT),              34
     *(NAMES_MOD5(17),NMOD5_GRHGT)                                        34
      EQUIVALENCE (COMMEN_MOD5(17),CMOD5_GRHGT),(MAP_MOD5(1,17),          34
     *MMOD5_GRHGT)                                                        34
      CHARACTER CMOD5_GRHGT*40/' '/                                       34
      CHARACTER NMOD5_GRHGT*12/'R.GRHGT'/                                 34
      INTEGER MMOD5_GRHGT(2)/1,1/                                         34
      REAL RE_MOD5_GRHGT                                                  34
C -                                                                       34
      REAL MOD5_GRWID                                                     34
      EQUIVALENCE (BANK_MOD5(113),MOD5_GRWID,RE_MOD5_GRWID),              34
     *(NAMES_MOD5(18),NMOD5_GRWID)                                        34
      EQUIVALENCE (COMMEN_MOD5(18),CMOD5_GRWID),(MAP_MOD5(1,18),          34
     *MMOD5_GRWID)                                                        34
      CHARACTER CMOD5_GRWID*40/' '/                                       34
      CHARACTER NMOD5_GRWID*12/'R.GRWID'/                                 34
      INTEGER MMOD5_GRWID(2)/1,1/                                         34
      REAL RE_MOD5_GRWID                                                  34
C -                                                                       34
      REAL MOD5_GRLEN                                                     34
      EQUIVALENCE (BANK_MOD5(114),MOD5_GRLEN,RE_MOD5_GRLEN),              34
     *(NAMES_MOD5(19),NMOD5_GRLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(19),CMOD5_GRLEN),(MAP_MOD5(1,19),          34
     *MMOD5_GRLEN)                                                        34
      CHARACTER CMOD5_GRLEN*40/' '/                                       34
      CHARACTER NMOD5_GRLEN*12/'R.GRLEN'/                                 34
      INTEGER MMOD5_GRLEN(2)/1,1/                                         34
      REAL RE_MOD5_GRLEN                                                  34
C -                                                                       34
      REAL MOD5_OGHGT                                                     34
      EQUIVALENCE (BANK_MOD5(115),MOD5_OGHGT,RE_MOD5_OGHGT),              34
     *(NAMES_MOD5(20),NMOD5_OGHGT)                                        34
      EQUIVALENCE (COMMEN_MOD5(20),CMOD5_OGHGT),(MAP_MOD5(1,20),          34
     *MMOD5_OGHGT)                                                        34
      CHARACTER CMOD5_OGHGT*40/' '/                                       34
      CHARACTER NMOD5_OGHGT*12/'R.OGHGT'/                                 34
      INTEGER MMOD5_OGHGT(2)/1,1/                                         34
      REAL RE_MOD5_OGHGT                                                  34
C -                                                                       34
      REAL MOD5_OGWID                                                     34
      EQUIVALENCE (BANK_MOD5(116),MOD5_OGWID,RE_MOD5_OGWID),              34
     *(NAMES_MOD5(21),NMOD5_OGWID)                                        34
      EQUIVALENCE (COMMEN_MOD5(21),CMOD5_OGWID),(MAP_MOD5(1,21),          34
     *MMOD5_OGWID)                                                        34
      CHARACTER CMOD5_OGWID*40/' '/                                       34
      CHARACTER NMOD5_OGWID*12/'R.OGWID'/                                 34
      INTEGER MMOD5_OGWID(2)/1,1/                                         34
      REAL RE_MOD5_OGWID                                                  34
C -                                                                       34
      REAL MOD5_OGLEN                                                     34
      EQUIVALENCE (BANK_MOD5(117),MOD5_OGLEN,RE_MOD5_OGLEN),              34
     *(NAMES_MOD5(22),NMOD5_OGLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(22),CMOD5_OGLEN),(MAP_MOD5(1,22),          34
     *MMOD5_OGLEN)                                                        34
      CHARACTER CMOD5_OGLEN*40/' '/                                       34
      CHARACTER NMOD5_OGLEN*12/'R.OGLEN'/                                 34
      INTEGER MMOD5_OGLEN(2)/1,1/                                         34
      REAL RE_MOD5_OGLEN                                                  34
C -                                                                       34
      REAL MOD5_IGHGT                                                     34
      EQUIVALENCE (BANK_MOD5(118),MOD5_IGHGT,RE_MOD5_IGHGT),              34
     *(NAMES_MOD5(23),NMOD5_IGHGT)                                        34
      EQUIVALENCE (COMMEN_MOD5(23),CMOD5_IGHGT),(MAP_MOD5(1,23),          34
     *MMOD5_IGHGT)                                                        34
      CHARACTER CMOD5_IGHGT*40/' '/                                       34
      CHARACTER NMOD5_IGHGT*12/'R.IGHGT'/                                 34
      INTEGER MMOD5_IGHGT(2)/1,1/                                         34
      REAL RE_MOD5_IGHGT                                                  34
C -                                                                       34
      REAL MOD5_IGWID                                                     34
      EQUIVALENCE (BANK_MOD5(119),MOD5_IGWID,RE_MOD5_IGWID),              34
     *(NAMES_MOD5(24),NMOD5_IGWID)                                        34
      EQUIVALENCE (COMMEN_MOD5(24),CMOD5_IGWID),(MAP_MOD5(1,24),          34
     *MMOD5_IGWID)                                                        34
      CHARACTER CMOD5_IGWID*40/' '/                                       34
      CHARACTER NMOD5_IGWID*12/'R.IGWID'/                                 34
      INTEGER MMOD5_IGWID(2)/1,1/                                         34
      REAL RE_MOD5_IGWID                                                  34
C -                                                                       34
      REAL MOD5_IGLEN                                                     34
      EQUIVALENCE (BANK_MOD5(120),MOD5_IGLEN,RE_MOD5_IGLEN),              34
     *(NAMES_MOD5(25),NMOD5_IGLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(25),CMOD5_IGLEN),(MAP_MOD5(1,25),          34
     *MMOD5_IGLEN)                                                        34
      CHARACTER CMOD5_IGLEN*40/' '/                                       34
      CHARACTER NMOD5_IGLEN*12/'R.IGLEN'/                                 34
      INTEGER MMOD5_IGLEN(2)/1,1/                                         34
      REAL RE_MOD5_IGLEN                                                  34
C -                                                                       34
      REAL MOD5_SPRMIN                                                    34
      EQUIVALENCE (BANK_MOD5(121),MOD5_SPRMIN,RE_MOD5_SPRMIN),            34
     *(NAMES_MOD5(26),NMOD5_SPRMIN)                                       34
      EQUIVALENCE (COMMEN_MOD5(26),CMOD5_SPRMIN),(MAP_MOD5(1,26),         34
     *MMOD5_SPRMIN)                                                       34
      CHARACTER CMOD5_SPRMIN*40/' '/                                      34
      CHARACTER NMOD5_SPRMIN*12/'R.SPRMIN'/                               34
      INTEGER MMOD5_SPRMIN(2)/1,1/                                        34
      REAL RE_MOD5_SPRMIN                                                 34
C -                                                                       34
      REAL MOD5_SPRMAX                                                    34
      EQUIVALENCE (BANK_MOD5(122),MOD5_SPRMAX,RE_MOD5_SPRMAX),            34
     *(NAMES_MOD5(27),NMOD5_SPRMAX)                                       34
      EQUIVALENCE (COMMEN_MOD5(27),CMOD5_SPRMAX),(MAP_MOD5(1,27),         34
     *MMOD5_SPRMAX)                                                       34
      CHARACTER CMOD5_SPRMAX*40/' '/                                      34
      CHARACTER NMOD5_SPRMAX*12/'R.SPRMAX'/                               34
      INTEGER MMOD5_SPRMAX(2)/1,1/                                        34
      REAL RE_MOD5_SPRMAX                                                 34
C -                                                                       34
      REAL MOD5_SPLEN                                                     34
      EQUIVALENCE (BANK_MOD5(123),MOD5_SPLEN,RE_MOD5_SPLEN),              34
     *(NAMES_MOD5(28),NMOD5_SPLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(28),CMOD5_SPLEN),(MAP_MOD5(1,28),          34
     *MMOD5_SPLEN)                                                        34
      CHARACTER CMOD5_SPLEN*40/' '/                                       34
      CHARACTER NMOD5_SPLEN*12/'R.SPLEN'/                                 34
      INTEGER MMOD5_SPLEN(2)/1,1/                                         34
      REAL RE_MOD5_SPLEN                                                  34
C -                                                                       34
      REAL MOD5_WGRMIN                                                    34
      EQUIVALENCE (BANK_MOD5(124),MOD5_WGRMIN,RE_MOD5_WGRMIN),            34
     *(NAMES_MOD5(29),NMOD5_WGRMIN)                                       34
      EQUIVALENCE (COMMEN_MOD5(29),CMOD5_WGRMIN),(MAP_MOD5(1,29),         34
     *MMOD5_WGRMIN)                                                       34
      CHARACTER CMOD5_WGRMIN*40/' '/                                      34
      CHARACTER NMOD5_WGRMIN*12/'R.WGRMIN'/                               34
      INTEGER MMOD5_WGRMIN(2)/1,1/                                        34
      REAL RE_MOD5_WGRMIN                                                 34
C -                                                                       34
      REAL MOD5_WGRMAX                                                    34
      EQUIVALENCE (BANK_MOD5(125),MOD5_WGRMAX,RE_MOD5_WGRMAX),            34
     *(NAMES_MOD5(30),NMOD5_WGRMAX)                                       34
      EQUIVALENCE (COMMEN_MOD5(30),CMOD5_WGRMAX),(MAP_MOD5(1,30),         34
     *MMOD5_WGRMAX)                                                       34
      CHARACTER CMOD5_WGRMAX*40/' '/                                      34
      CHARACTER NMOD5_WGRMAX*12/'R.WGRMAX'/                               34
      INTEGER MMOD5_WGRMAX(2)/1,1/                                        34
      REAL RE_MOD5_WGRMAX                                                 34
C -                                                                       34
      REAL MOD5_WGLEN                                                     34
      EQUIVALENCE (BANK_MOD5(126),MOD5_WGLEN,RE_MOD5_WGLEN),              34
     *(NAMES_MOD5(31),NMOD5_WGLEN)                                        34
      EQUIVALENCE (COMMEN_MOD5(31),CMOD5_WGLEN),(MAP_MOD5(1,31),          34
     *MMOD5_WGLEN)                                                        34
      CHARACTER CMOD5_WGLEN*40/' '/                                       34
      CHARACTER NMOD5_WGLEN*12/'R.WGLEN'/                                 34
      INTEGER MMOD5_WGLEN(2)/1,1/                                         34
      REAL RE_MOD5_WGLEN                                                  34
C -                                                                       34
      REAL MOD5_FEEH                                                      34
      EQUIVALENCE (BANK_MOD5(127),MOD5_FEEH,RE_MOD5_FEEH),                34
     *(NAMES_MOD5(32),NMOD5_FEEH)                                         34
      EQUIVALENCE (COMMEN_MOD5(32),CMOD5_FEEH),(MAP_MOD5(1,32),           34
     *MMOD5_FEEH)                                                         34
      CHARACTER CMOD5_FEEH*40/' '/                                        34
      CHARACTER NMOD5_FEEH*12/'R.FEEH'/                                   34
      INTEGER MMOD5_FEEH(2)/1,1/                                          34
      REAL RE_MOD5_FEEH                                                   34
C -                                                                       34
      REAL MOD5_HBWID                                                     34
      EQUIVALENCE (BANK_MOD5(128),MOD5_HBWID,RE_MOD5_HBWID),              34
     *(NAMES_MOD5(33),NMOD5_HBWID)                                        34
      EQUIVALENCE (COMMEN_MOD5(33),CMOD5_HBWID),(MAP_MOD5(1,33),          34
     *MMOD5_HBWID)                                                        34
      CHARACTER CMOD5_HBWID*40/' '/                                       34
      CHARACTER NMOD5_HBWID*12/'R.HBWID'/                                 34
      INTEGER MMOD5_HBWID(2)/1,1/                                         34
      REAL RE_MOD5_HBWID                                                  34
C -                                                                       34
      REAL MOD5_NGAP                                                      34
      EQUIVALENCE (BANK_MOD5(129),MOD5_NGAP,RE_MOD5_NGAP),                34
     *(NAMES_MOD5(34),NMOD5_NGAP)                                         34
      EQUIVALENCE (COMMEN_MOD5(34),CMOD5_NGAP),(MAP_MOD5(1,34),           34
     *MMOD5_NGAP)                                                         34
      CHARACTER CMOD5_NGAP*40/' '/                                        34
      CHARACTER NMOD5_NGAP*12/'R.NGAP'/                                   34
      INTEGER MMOD5_NGAP(2)/1,1/                                          34
      REAL RE_MOD5_NGAP                                                   34
C -                                                                       34
      REAL MOD5_TRAYEDGEZ                                                 34
      EQUIVALENCE (BANK_MOD5(130),MOD5_TRAYEDGEZ,RE_MOD5_TRAYEDGEZ),      34
     *(NAMES_MOD5(35),NMOD5_TRAYEDGEZ)                                    34
      EQUIVALENCE (COMMEN_MOD5(35),CMOD5_TRAYEDGEZ),(MAP_MOD5(1,35),      34
     *MMOD5_TRAYEDGEZ)                                                    34
      CHARACTER CMOD5_TRAYEDGEZ*40/' '/                                   34
      CHARACTER NMOD5_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         34
      INTEGER MMOD5_TRAYEDGEZ(2)/1,1/                                     34
      REAL RE_MOD5_TRAYEDGEZ                                              34
C -                                                                       34
      INTEGER LEN_MOD5,LLEN_MOD5,LINK_MOD5                                34
      PARAMETER (LEN_MOD5=130)                                            34
      PARAMETER (LLEN_MOD5=35)                                            34
      INTEGER FLAG_MOD5/0/,MAP_MOD5(2,LLEN_MOD5),IDNUM_MOD5(10)           34
      CHARACTER NAMES_MOD5(LLEN_MOD5)*12,COMMEN_MOD5(LLEN_MOD5)*40        34
      DATA IDNUM_MOD5/10*0/                                               34
      REAL BANK_MOD5(LEN_MOD5)                                            34
      CHARACTER*60 BPATH_MOD5/' '/                                        34
      SAVE FLAG_MOD5,MAP_MOD5,IDNUM_MOD5,NAMES_MOD5,COMMEN_MOD5,          34
     *BPATH_MOD5                                                          34
      DATA LINK_MOD5/0/,BANK_MOD5/LEN_MOD5*0/                             34
C *                                                                       34
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           36
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        36
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       36
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP, YEAR                             38
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            40
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               40
      BPATH_BTOG='BTOG*'                                                  40
      IDNUM_BTOG(1)=0                                                     41
C GEOMETRY VERSION                                                        41
      BTOG_VERSION=6                                                      41
      CBTOG_VERSION='GEOMETRY VERSION'                                    41
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                42
      BTOG_RMIN=207.80                                                    42
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               42
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           43
      BTOG_RMAX=219.5                                                     43
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          43
C CTB/TOF TUBE HALF LENGTH                                                44
      BTOG_DZ=246.0                                                       44
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 44
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, ! 5=1 TRAY-TOFR, 6=FUL   45
      BTOG_CHOICE=8                                                       45
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, ! 5=1  45
     * TRAY-TOFR, 6=FUL'                                                  45
C TOFP TRAY POSITION: (0) CHOICE 4 OR 5 -> RUN-2,3 POSN, !                46
      BTOG_POSIT1(1)=32                                                   46
      BTOG_POSIT1(2)=33                                                   46
      CBTOG_POSIT1='TOFP TRAY POSITION: (0) CHOICE 4 OR 5 -> RUN-2,3 POS  46
     *N, !'                                                               46
C TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN                           47
      BTOG_POSIT2=23                                                      47
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN'        47
C TOFR TRAY POSITION FOR CHOICE 8 -> RUN-5 POSN                           48
      BTOG_POSIT3=33                                                      48
      CBTOG_POSIT3='TOFR TRAY POSITION FOR CHOICE 8 -> RUN-5 POSN'        48
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    49
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           49
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   49
C *                                                                       49
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    49
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           49
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   49
C *                                                                       50
      CALL AGSBEGM(MODULE,IPRIN)                                          50
      IF (IPRIN.LT.0) RETURN                                              50
C Filling bank TRAY - GENERAL TRAY STATS                                  50
      BANK_TITLE='GENERAL TRAY STATS'                                     50
      BPATH_TRAY='TRAY*'                                                  50
      IDNUM_TRAY(1)=0                                                     51
C TRAY HEIGHT(8.89)                                                       51
      TRAY_HEIGHT=8.89                                                    51
      CTRAY_HEIGHT='TRAY HEIGHT(8.89)'                                    51
C FULL TRAY WIDTH                                                         52
      TRAY_WIDTH=21.59                                                    52
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       52
C FULL TRAY LENGTH(241.62)                                                53
      TRAY_LENGTH=241.62                                                  53
      CTRAY_LENGTH='FULL TRAY LENGTH(241.62)'                             53
C TRAY WALL THICKNESS                                                     54
      TRAY_WALLTHK=0.13                                                   54
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 54
C SUPPORT HEIGHT (RADIAL)                                                 55
      TRAY_SUPFULLH=2.03                                                  55
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            55
C SUPPORT FULL WIDTH WITH ARMS                                            56
      TRAY_SUPFULLW=15.24                                                 56
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       56
C SUPPORT LENGTH                                                          57
      TRAY_SUPLEN=215.9                                                   57
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       57
C SUPPORT BASE WIDTH                                                      58
      TRAY_SUPBASEW=9.22                                                  58
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 58
C SUPPORT BASE THICKNESS                                                  59
      TRAY_SUPBASET=0.32                                                  59
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             59
C SUPPORT ARM THICKNESS                                                   60
      TRAY_SUPARMT=0.64                                                   60
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               60
C COOLING CHANNEL OUTER RADIUS                                            61
      TRAY_COOLOUTR=0.80                                                  61
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       61
C COOLING CHANNEL INNER RADIUS                                            62
      TRAY_COOLINNR=0.48                                                  62
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       62
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               63
      TRAY_STRIPT=0.08                                                    63
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            63
C FOOT INSET FROM TRAY EDGE                                               64
      TRAY_FOOTINSE=1.06                                                  64
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          64
C THICKNESS OF FOOT MATERIAL                                              65
      TRAY_FOOTTHK=0.23                                                   65
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          65
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               66
      TRAY_FOOT1LEN=1.68                                                  66
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          66
C THICKNESS OF SECOND FOOT SECTION                                        67
      TRAY_FOOT2THK=1.16                                                  67
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   67
C LENGTH OF THIRD PART OF FOOT                                            68
      TRAY_FOOT3LEN=2.16                                                  68
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       68
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    69
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           69
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   69
C *                                                                       69
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    69
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           69
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   69
C *                                                                       70
      CALL AGSBEGM(MODULE,IPRIN)                                          70
      IF (IPRIN.LT.0) RETURN                                              70
C Filling bank CTBB - BARREL TRIGGER STATS                                70
      BANK_TITLE='BARREL TRIGGER STATS'                                   70
      BPATH_CTBB='CTBB*'                                                  70
      IDNUM_CTBB(1)=0                                                     71
C FIRST SLAB (B) LENGTH                                                   71
      CTBB_SLAB1LEN=112.5                                                 71
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              71
C SECOND SLAB (A)LENGTH                                                   72
      CTBB_SLAB2LEN=130.0                                                 72
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              72
C FIRST SLAB (B) X POSITION                                               73
      CTBB_SLAB1X=5.84                                                    73
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            73
C SECOND SLAB (A) X POSITION                                              74
      CTBB_SLAB2X=2.67                                                    74
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           74
C SCINTILLATOR SLAB THICKNESSES                                           75
      CTBB_SLABTHCK=1.0                                                   75
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      75
C SCINTILLATOR SLAB WIDTH                                                 76
      CTBB_SLABWID=21.0                                                   76
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             76
C OPTICAL CONVERTER LENGTH                                                77
      CTBB_CONVLEN=8.5                                                    77
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            77
C OPTICAL CONVERTOR MIN WIDTH                                             78
      CTBB_CONVWIDM=4.0                                                   78
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        78
C OPTICAL CONVERTOR THICKNESS                                             79
      CTBB_CONVTHCK=0.92                                                  79
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        79
C PMT LENGTH                                                              80
      CTBB_PMTLEN=5.0                                                     80
      CCTBB_PMTLEN='PMT LENGTH'                                           80
C PMT MAX RADIUS                                                          81
      CTBB_PMTMAXR=2.0                                                    81
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      81
C PMT MIN RADIUS                                                          82
      CTBB_PMTMINR=1.84                                                   82
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      82
C BASE LENGTH                                                             83
      CTBB_BASELEN=4.0                                                    83
      CCTBB_BASELEN='BASE LENGTH'                                         83
C BASE MAX RADIUS                                                         84
      CTBB_BASEMAXR=2.13                                                  84
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    84
C BASE MIN RADIUS                                                         85
      CTBB_BASEMINR=1.0                                                   85
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    85
C READOUT ELECTRONICS THICKNESS                                           86
      CTBB_ELECTHCK=0.25                                                  86
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      86
C THICKNESS OF TYVEK + BLACK PLASTIC                                      87
      CTBB_WRAP=0.13                                                      87
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     87
C THICKNESS OF SHIM TO POSITION SLAT 2                                    88
      CTBB_SHIM=0.26                                                      88
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   88
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    89
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           89
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   89
C *                                                                       89
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    89
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           89
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   89
C *                                                                       90
      CALL AGSBEGM(MODULE,IPRIN)                                          90
      IF (IPRIN.LT.0) RETURN                                              90
C Filling bank TOFF - TIME OF FLIGHT STATS                                90
      BANK_TITLE='TIME OF FLIGHT STATS'                                   90
      BPATH_TOFF='TOFF*'                                                  90
      IDNUM_TOFF(1)=0                                                     91
C WIDTH OF THE 5W BOX (BMTD)                                              91
      TOFF_BOXWIDTH=21.0875                                               91
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         91
C SLAT LENGTH                                                             92
      TOFF_SLATLEN=20.0                                                   92
      CTOFF_SLATLEN='SLAT LENGTH'                                         92
C 5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD                           93
      TOFF_SLAT01Z=104.938                                                93
      CTOFF_SLAT01Z='5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD'       93
C 4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD                           94
      TOFF_SLAT02Z=84.060                                                 94
      CTOFF_SLAT02Z='4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD'       94
C 4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD                           95
      TOFF_SLAT03Z=62.860                                                 95
      CTOFF_SLAT03Z='4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD'       95
C 4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD                           96
      TOFF_SLAT04Z=41.254                                                 96
      CTOFF_SLAT04Z='4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD'       96
C 4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD                           97
      TOFF_SLAT05Z=18.966                                                 97
      CTOFF_SLAT05Z='4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD'       97
C 4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD                           98
      TOFF_SLAT06Z=-3.954                                                 98
      CTOFF_SLAT06Z='4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD'       98
C 4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD                           99
      TOFF_SLAT07Z=-27.528                                                99
      CTOFF_SLAT07Z='4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD'       99
C 4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD                           100
      TOFF_SLAT08Z=-51.254                                                100
      CTOFF_SLAT08Z='4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD'       100
C 4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD                           101
      TOFF_SLAT09Z=-75.634                                                101
      CTOFF_SLAT09Z='4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD'       101
C 4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD                          102
      TOFF_SLAT10Z=-100.683                                               102
      CTOFF_SLAT10Z='4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD'      102
C SCINTILLATOR SLAB THICKNESSES                                           103
      TOFF_SLATTHCK=2.0                                                   103
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      103
C SCINTILLATOR SLAB WIDTH (4.0)                                           104
      TOFF_SLATWID=3.81                                                   104
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       104
C SLAT ASSY. ANGLE W.R.T. TRAY                                            105
      TOFF_SLATANG=11.5                                                   105
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        105
C PMT LENGTH                                                              106
      TOFF_PMTLEN=5.0                                                     106
      CTOFF_PMTLEN='PMT LENGTH'                                           106
C PMT MAX RADIUS                                                          107
      TOFF_PMTMAXR=1.91                                                   107
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      107
C PMT MIN RADIUS                                                          108
      TOFF_PMTMINR=1.8                                                    108
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      108
C THICKNESS OF SOCKET                                                     109
      TOFF_SOCKLEN=1.0                                                    109
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 109
C BASE LENGTH                                                             110
      TOFF_BASELEN=5.0                                                    110
      CTOFF_BASELEN='BASE LENGTH'                                         110
C BASE MAX RADIUS                                                         111
      TOFF_BASEMAXR=1.91                                                  111
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    111
C BASE MIN RADIUS                                                         112
      TOFF_BASEMINR=1.8                                                   112
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    112
C CELL WIDTH                                                              113
      TOFF_CELLWID=3.1                                                    113
      CTOFF_CELLWID='CELL WIDTH'                                          113
C CELL HEIGHT                                                             114
      TOFF_CELLHGT=1.6                                                    114
      CTOFF_CELLHGT='CELL HEIGHT'                                         114
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           115
      TOFF_ELECHGT=3.0                                                    115
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       115
C FEE BOARD THICKNESS (67 MILS)                                           116
      TOFF_ELECTHCK=0.17                                                  116
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      116
C FEE BOARD WIDTH (WAS 21)                                                117
      TOFF_ELECWID=20.3                                                   117
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            117
C FEE BOARD LENGTH (WAS 16)                                               118
      TOFF_ELECLEN=5.715                                                  118
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           118
C FEE Z POSITION FOR ROW 1 FROM AUTOCAD                                   119
      TOFF_ELEC01Z=105.610                                                119
      CTOFF_ELEC01Z='FEE Z POSITION FOR ROW 1 FROM AUTOCAD'               119
C FEE Z POSITION FOR ROW 2 FROM AUTOCAD                                   120
      TOFF_ELEC02Z=84.573                                                 120
      CTOFF_ELEC02Z='FEE Z POSITION FOR ROW 2 FROM AUTOCAD'               120
C FEE Z POSITION FOR ROW 3 FROM AUTOCAD                                   121
      TOFF_ELEC03Z=63.224                                                 121
      CTOFF_ELEC03Z='FEE Z POSITION FOR ROW 3 FROM AUTOCAD'               121
C FEE Z POSITION FOR ROW 4 FROM AUTOCAD                                   122
      TOFF_ELEC04Z=41.667                                                 122
      CTOFF_ELEC04Z='FEE Z POSITION FOR ROW 4 FROM AUTOCAD'               122
C FEE Z POSITION FOR ROW 5 FROM AUTOCAD                                   123
      TOFF_ELEC05Z=19.379                                                 123
      CTOFF_ELEC05Z='FEE Z POSITION FOR ROW 5 FROM AUTOCAD'               123
C FEE Z POSITION FOR ROW 6 FROM AUTOCAD                                   124
      TOFF_ELEC06Z=-3.542                                                 124
      CTOFF_ELEC06Z='FEE Z POSITION FOR ROW 6 FROM AUTOCAD'               124
C FEE Z POSITION FOR ROW 7 FROM AUTOCAD                                   125
      TOFF_ELEC07Z=-27.165                                                125
      CTOFF_ELEC07Z='FEE Z POSITION FOR ROW 7 FROM AUTOCAD'               125
C FEE Z POSITION FOR ROW 8 FROM AUTOCAD                                   126
      TOFF_ELEC08Z=-50.841                                                126
      CTOFF_ELEC08Z='FEE Z POSITION FOR ROW 8 FROM AUTOCAD'               126
C FEE Z POSITION FOR ROW 9 FROM AUTOCAD                                   127
      TOFF_ELEC09Z=-75.170                                                127
      CTOFF_ELEC09Z='FEE Z POSITION FOR ROW 9 FROM AUTOCAD'               127
C FEE Z POSITION FOR ROW 10 FROM AUTOCAD                                  128
      TOFF_ELEC10Z=-100.270                                               128
      CTOFF_ELEC10Z='FEE Z POSITION FOR ROW 10 FROM AUTOCAD'              128
C COOLING LOOP RAIL THICKNESS                                             129
      TOFF_RAILTHCK=0.2                                                   129
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        129
C COOLING LOOP RAIL WIDTH                                                 130
      TOFF_RAILWID=1.5                                                    130
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             130
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 131
      TOFF_COOLOUTR=0.635                                                 131
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            131
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       132
      TOFF_COOLINNR=0.561                                                 132
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    132
     *2'                                                                  132
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    133
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           133
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   133
C *                                                                       133
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    133
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           133
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   133
C *                                                                       134
      CALL AGSBEGM(MODULE,IPRIN)                                          134
      IF (IPRIN.LT.0) RETURN                                              134
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       134
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          134
      BPATH_MODR='MODR*'                                                  134
      IDNUM_MODR(1)=0                                                     135
C MODULE HEIGHT (R)                                                       135
      MODR_HEIGHT=1.95                                                    135
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    135
C MODULE WIDTH (PHI)                                                      136
      MODR_WIDTH=21.2                                                     136
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    136
C MODULE LENGTH (Z)                                                       137
      MODR_LENGTH=9.4                                                     137
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    137
C MODULE DETECTOR CENTER IN (PHI)                                         138
      MODR_CENTER=0.35                                                    138
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      138
C MRPC XPOSITIONS                                                         144
      MODR_MRPCX(1)=4.76                                                  144
      MODR_MRPCX(2)=1.21                                                  144
      MODR_MRPCX(3)=4.99                                                  144
      MODR_MRPCX(4)=1.46                                                  144
      MODR_MRPCX(5)=5.19                                                  144
      MODR_MRPCX(6)=1.71                                                  144
      MODR_MRPCX(7)=4.98                                                  144
      MODR_MRPCX(8)=1.54                                                  144
      MODR_MRPCX(9)=2.69                                                  144
      MODR_MRPCX(10)=3.39                                                 144
      MODR_MRPCX(11)=3.25                                                 144
      MODR_MRPCX(12)=3.49                                                 144
      MODR_MRPCX(13)=3.33                                                 144
      MODR_MRPCX(14)=3.54                                                 144
      MODR_MRPCX(15)=3.54                                                 144
      MODR_MRPCX(16)=3.57                                                 144
      MODR_MRPCX(17)=3.48                                                 144
      MODR_MRPCX(18)=3.51                                                 144
      MODR_MRPCX(19)=3.19                                                 144
      MODR_MRPCX(20)=3.19                                                 144
      MODR_MRPCX(21)=3.19                                                 144
      MODR_MRPCX(22)=3.19                                                 144
      MODR_MRPCX(23)=3.19                                                 144
      MODR_MRPCX(24)=3.43                                                 144
      MODR_MRPCX(25)=3.43                                                 144
      MODR_MRPCX(26)=3.43                                                 144
      MODR_MRPCX(27)=3.43                                                 144
      MODR_MRPCX(28)=3.43                                                 144
      MODR_MRPCX(29)=3.43                                                 144
      MODR_MRPCX(30)=3.43                                                 144
      MODR_MRPCX(31)=3.43                                                 144
      MODR_MRPCX(32)=3.43                                                 144
      MODR_MRPCX(33)=3.43                                                 144
      CMODR_MRPCX='MRPC XPOSITIONS'                                       144
C MRPC ZPOSITIONS                                                         151
      MODR_MRPCZ(1)=4.50                                                  151
      MODR_MRPCZ(2)=10.48                                                 151
      MODR_MRPCZ(3)=16.83                                                 151
      MODR_MRPCZ(4)=22.64                                                 151
      MODR_MRPCZ(5)=29.25                                                 151
      MODR_MRPCZ(6)=34.89                                                 151
      MODR_MRPCZ(7)=41.68                                                 151
      MODR_MRPCZ(8)=47.19                                                 151
      MODR_MRPCZ(9)=53.76                                                 151
      MODR_MRPCZ(10)=60.18                                                151
      MODR_MRPCZ(11)=66.53                                                151
      MODR_MRPCZ(12)=72.95                                                151
      MODR_MRPCZ(13)=79.42                                                151
      MODR_MRPCZ(14)=85.97                                                151
      MODR_MRPCZ(15)=92.59                                                151
      MODR_MRPCZ(16)=99.28                                                151
      MODR_MRPCZ(17)=106.00                                               151
      MODR_MRPCZ(18)=112.84                                               151
      MODR_MRPCZ(19)=119.60                                               151
      MODR_MRPCZ(20)=126.66                                               151
      MODR_MRPCZ(21)=133.80                                               151
      MODR_MRPCZ(22)=141.06                                               151
      MODR_MRPCZ(23)=148.43                                               151
      MODR_MRPCZ(24)=156.09                                               151
      MODR_MRPCZ(25)=163.71                                               151
      MODR_MRPCZ(26)=171.46                                               151
      MODR_MRPCZ(27)=179.35                                               151
      MODR_MRPCZ(28)=187.38                                               151
      MODR_MRPCZ(29)=195.56                                               151
      MODR_MRPCZ(30)=203.84                                               151
      MODR_MRPCZ(31)=212.33                                               151
      MODR_MRPCZ(32)=220.94                                               151
      MODR_MRPCZ(33)=229.7                                                151
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       151
C MRPC ANGLES                                                             157
      MODR_MRPCA(1)=1.20                                                  157
      MODR_MRPCA(2)=2.84                                                  157
      MODR_MRPCA(3)=4.48                                                  157
      MODR_MRPCA(4)=6.12                                                  157
      MODR_MRPCA(5)=7.74                                                  157
      MODR_MRPCA(6)=9.36                                                  157
      MODR_MRPCA(7)=10.97                                                 157
      MODR_MRPCA(8)=7.60                                                  157
      MODR_MRPCA(9)=14.30                                                 157
      MODR_MRPCA(10)=22.30                                                157
      MODR_MRPCA(11)=22.30                                                157
      MODR_MRPCA(12)=22.30                                                157
      MODR_MRPCA(13)=22.30                                                157
      MODR_MRPCA(14)=22.30                                                157
      MODR_MRPCA(15)=22.30                                                157
      MODR_MRPCA(16)=22.30                                                157
      MODR_MRPCA(17)=22.30                                                157
      MODR_MRPCA(18)=22.30                                                157
      MODR_MRPCA(19)=33.00                                                157
      MODR_MRPCA(20)=33.00                                                157
      MODR_MRPCA(21)=33.00                                                157
      MODR_MRPCA(22)=33.00                                                157
      MODR_MRPCA(23)=33.00                                                157
      MODR_MRPCA(24)=37.58                                                157
      MODR_MRPCA(25)=37.58                                                157
      MODR_MRPCA(26)=37.58                                                157
      MODR_MRPCA(27)=37.58                                                157
      MODR_MRPCA(28)=37.58                                                157
      MODR_MRPCA(29)=37.58                                                157
      MODR_MRPCA(30)=37.58                                                157
      MODR_MRPCA(31)=37.58                                                157
      MODR_MRPCA(32)=37.58                                                157
      MODR_MRPCA(33)=37.58                                                157
      CMODR_MRPCA='MRPC ANGLES'                                           157
C HC->HEIGHT (R)                                                          158
      MODR_HCHGT=0.466                                                    158
      CMODR_HCHGT='HC->HEIGHT (R)'                                        158
C HC->WIDTH (PHI)                                                         159
      MODR_HCWID=20.8                                                     159
      CMODR_HCWID='HC->WIDTH (PHI)'                                       159
C HC->LENGTH (Z)                                                          160
      MODR_HCLEN=8.4                                                      160
      CMODR_HCLEN='HC->LENGTH (Z)'                                        160
C PCB->HEIGHT (R)                                                         161
      MODR_PCBHGT=0.15                                                    161
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      161
C PCB->WIDTH (PHI)                                                        162
      MODR_PCBWID=21.0                                                    162
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     162
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     163
      MODR_PCBLEN=9.4                                                     163
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  163
C MYLAR->HEIGHT                                                           164
      MODR_MYHGT=0.035                                                    164
      CMODR_MYHGT='MYLAR->HEIGHT'                                         164
C MYLAR->WIDTH                                                            165
      MODR_MYWID=21.2                                                     165
      CMODR_MYWID='MYLAR->WIDTH'                                          165
C MYLAR->LENGTH                                                           166
      MODR_MYLEN=8.4                                                      166
      CMODR_MYLEN='MYLAR->LENGTH'                                         166
C GRAPHITE->HEIGHT                                                        167
      MODR_GRHGT=0.013                                                    167
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      167
C GRAPHITE->WIDTH                                                         168
      MODR_GRWID=20.2                                                     168
      CMODR_GRWID='GRAPHITE->WIDTH'                                       168
C GRAPHITE->LENGTH                                                        169
      MODR_GRLEN=7.4                                                      169
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      169
C OUTER GLASS->HEIGHT                                                     170
      MODR_OGHGT=0.11                                                     170
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   170
C OUTER GLASS->WIDTH                                                      171
      MODR_OGWID=20.6                                                     171
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    171
C OUTER GLASS->LENGTH                                                     172
      MODR_OGLEN=7.8                                                      172
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   172
C INNER GLASS->HEIGHT                                                     173
      MODR_IGHGT=0.054                                                    173
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   173
C INNER GLASS->WIDTH                                                      174
      MODR_IGWID=20.0                                                     174
      CMODR_IGWID='INNER GLASS->WIDTH'                                    174
C INNER GLASS->LENGTH                                                     175
      MODR_IGLEN=6.1                                                      175
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   175
C SEPARATOR TUBE->RMIN                                                    176
      MODR_SPRMIN=0.                                                      176
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 176
C SEPARATOR TUBE->RMAX                                                    177
      MODR_SPRMAX=0.011                                                   177
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 177
C SEPARATOR TUBE->LENGTH                                                  178
      MODR_SPLEN=7.8                                                      178
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                178
C WEDGE TUBE: RMIN                                                        179
      MODR_WGRMIN=0.                                                      179
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     179
C WEDGE TUBE->RMAX                                                        180
      MODR_WGRMAX=0.15                                                    180
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     180
C WEDGE TUBE->LENGTH                                                      181
      MODR_WGLEN=10.0                                                     181
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    181
C TOFR FEE PCB THICKNESS                                                  182
      MODR_FEEH=0.15                                                      182
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 182
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    183
      MODR_HBWID=0.635                                                    183
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  183
C NUMBER OF GAPS IN MRPC                                                  184
      MODR_NGAP=6                                                         184
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 184
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    185
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           185
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   185
C *                                                                       185
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    185
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           185
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   185
C *                                                                       186
      CALL AGSBEGM(MODULE,IPRIN)                                          186
      IF (IPRIN.LT.0) RETURN                                              186
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       186
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          186
      BPATH_MOD4='MOD4*'                                                  186
      IDNUM_MOD4(1)=0                                                     187
C MODULE HEIGHT (R)                                                       187
      MOD4_HEIGHT=1.95                                                    187
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    187
C MODULE WIDTH (PHI)                                                      188
      MOD4_WIDTH=21.2                                                     188
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    188
C MODULE LENGTH (Z)                                                       189
      MOD4_LENGTH=9.4                                                     189
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    189
C MODULE DETECTOR CENTER (PHI)                                            190
      MOD4_CENTER=0.35                                                    190
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         190
C MRPC ZPOSNS                                                             197
      MOD4_MRPCZ(1)=5.83                                                  197
      MOD4_MRPCZ(2)=11.97                                                 197
      MOD4_MRPCZ(3)=18.17                                                 197
      MOD4_MRPCZ(4)=24.03                                                 197
      MOD4_MRPCZ(5)=30.55                                                 197
      MOD4_MRPCZ(6)=36.22                                                 197
      MOD4_MRPCZ(7)=42.54                                                 197
      MOD4_MRPCZ(8)=48.87                                                 197
      MOD4_MRPCZ(9)=55.19                                                 197
      MOD4_MRPCZ(10)=61.53                                                197
      MOD4_MRPCZ(11)=67.86                                                197
      MOD4_MRPCZ(12)=74.24                                                197
      MOD4_MRPCZ(13)=80.68                                                197
      MOD4_MRPCZ(14)=87.08                                                197
      MOD4_MRPCZ(15)=93.64                                                197
      MOD4_MRPCZ(16)=100.36                                               197
      MOD4_MRPCZ(17)=107.19                                               197
      MOD4_MRPCZ(18)=114.05                                               197
      MOD4_MRPCZ(19)=121.02                                               197
      MOD4_MRPCZ(20)=128.08                                               197
      MOD4_MRPCZ(21)=135.24                                               197
      MOD4_MRPCZ(22)=142.51                                               197
      MOD4_MRPCZ(23)=149.88                                               197
      MOD4_MRPCZ(24)=157.35                                               197
      MOD4_MRPCZ(25)=164.93                                               197
      MOD4_MRPCZ(26)=172.63                                               197
      MOD4_MRPCZ(27)=180.43                                               197
      MOD4_MRPCZ(28)=188.35                                               197
      MOD4_MRPCZ(29)=196.38                                               197
      MOD4_MRPCZ(30)=204.52                                               197
      MOD4_MRPCZ(31)=212.79                                               197
      MOD4_MRPCZ(32)=221.17                                               197
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           197
C MRPC XPOSNS                                                             203
      MOD4_MRPCX(1)=1.5                                                   203
      MOD4_MRPCX(2)=2.83                                                  203
      MOD4_MRPCX(3)=4.16                                                  203
      MOD4_MRPCX(4)=1.55                                                  203
      MOD4_MRPCX(5)=4.73                                                  203
      MOD4_MRPCX(6)=1.88                                                  203
      MOD4_MRPCX(7)=2.56                                                  203
      MOD4_MRPCX(8)=3.06                                                  203
      MOD4_MRPCX(9)=3.3                                                   203
      MOD4_MRPCX(10)=3.41                                                 203
      MOD4_MRPCX(11)=3.32                                                 203
      MOD4_MRPCX(12)=3.2                                                  203
      MOD4_MRPCX(13)=3.07                                                 203
      MOD4_MRPCX(14)=2.73                                                 203
      MOD4_MRPCX(15)=2.58                                                 203
      MOD4_MRPCX(16)=2.64                                                 203
      MOD4_MRPCX(17)=2.74                                                 203
      MOD4_MRPCX(18)=2.74                                                 203
      MOD4_MRPCX(19)=2.76                                                 203
      MOD4_MRPCX(20)=2.76                                                 203
      MOD4_MRPCX(21)=2.76                                                 203
      MOD4_MRPCX(22)=2.76                                                 203
      MOD4_MRPCX(23)=2.76                                                 203
      MOD4_MRPCX(24)=2.76                                                 203
      MOD4_MRPCX(25)=2.76                                                 203
      MOD4_MRPCX(26)=2.76                                                 203
      MOD4_MRPCX(27)=2.76                                                 203
      MOD4_MRPCX(28)=2.76                                                 203
      MOD4_MRPCX(29)=2.76                                                 203
      MOD4_MRPCX(30)=2.76                                                 203
      MOD4_MRPCX(31)=2.76                                                 203
      MOD4_MRPCX(32)=2.76                                                 203
      CMOD4_MRPCX='MRPC XPOSNS'                                           203
C MRPC ANGLES                                                             207
      MOD4_MRPCA(1)=6.                                                    207
      MOD4_MRPCA(2)=6.                                                    207
      MOD4_MRPCA(3)=6.                                                    207
      MOD4_MRPCA(4)=6.                                                    207
      MOD4_MRPCA(5)=0.                                                    207
      MOD4_MRPCA(6)=12.                                                   207
      MOD4_MRPCA(7)=12.                                                   207
      MOD4_MRPCA(8)=16.                                                   207
      MOD4_MRPCA(9)=16.                                                   207
      MOD4_MRPCA(10)=19.                                                  207
      MOD4_MRPCA(11)=19.                                                  207
      MOD4_MRPCA(12)=19.                                                  207
      MOD4_MRPCA(13)=19.                                                  207
      MOD4_MRPCA(14)=24.                                                  207
      MOD4_MRPCA(15)=24.                                                  207
      MOD4_MRPCA(16)=25.                                                  207
      MOD4_MRPCA(17)=27.                                                  207
      MOD4_MRPCA(18)=29.                                                  207
      MOD4_MRPCA(19)=30.                                                  207
      MOD4_MRPCA(20)=30.                                                  207
      MOD4_MRPCA(21)=30.                                                  207
      MOD4_MRPCA(22)=30.                                                  207
      MOD4_MRPCA(23)=30.                                                  207
      MOD4_MRPCA(24)=30.                                                  207
      MOD4_MRPCA(25)=30.                                                  207
      MOD4_MRPCA(26)=30.                                                  207
      MOD4_MRPCA(27)=30.                                                  207
      MOD4_MRPCA(28)=30.                                                  207
      MOD4_MRPCA(29)=30.                                                  207
      MOD4_MRPCA(30)=30.                                                  207
      MOD4_MRPCA(31)=30.                                                  207
      MOD4_MRPCA(32)=30.                                                  207
      CMOD4_MRPCA='MRPC ANGLES'                                           207
C HC->HEIGHT (R)                                                          208
      MOD4_HCHGT=0.466                                                    208
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        208
C HC->WIDTH (PHI)                                                         209
      MOD4_HCWID=20.8                                                     209
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       209
C HC->LENGTH (Z)                                                          210
      MOD4_HCLEN=8.4                                                      210
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        210
C PCB->HEIGHT (R)                                                         211
      MOD4_PCBHGT=0.15                                                    211
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      211
C PCB->WIDTH (PHI)                                                        212
      MOD4_PCBWID=21.0                                                    212
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     212
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     213
      MOD4_PCBLEN=9.4                                                     213
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  213
C MYLAR->HEIGHT                                                           214
      MOD4_MYHGT=0.035                                                    214
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         214
C MYLAR->WIDTH                                                            215
      MOD4_MYWID=21.2                                                     215
      CMOD4_MYWID='MYLAR->WIDTH'                                          215
C MYLAR->LENGTH                                                           216
      MOD4_MYLEN=8.4                                                      216
      CMOD4_MYLEN='MYLAR->LENGTH'                                         216
C GRAPHITE->HEIGHT                                                        217
      MOD4_GRHGT=0.013                                                    217
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      217
C GRAPHITE->WIDTH                                                         218
      MOD4_GRWID=20.2                                                     218
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       218
C GRAPHITE->LENGTH                                                        219
      MOD4_GRLEN=7.4                                                      219
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      219
C OUTER GLASS->HEIGHT                                                     220
      MOD4_OGHGT=0.11                                                     220
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   220
C OUTER GLASS->WIDTH                                                      221
      MOD4_OGWID=20.6                                                     221
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    221
C OUTER GLASS->LENGTH                                                     222
      MOD4_OGLEN=7.8                                                      222
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   222
C INNER GLASS->HEIGHT                                                     223
      MOD4_IGHGT=0.054                                                    223
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   223
C INNER GLASS->WIDTH                                                      224
      MOD4_IGWID=20.0                                                     224
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    224
C INNER GLASS->LENGTH                                                     225
      MOD4_IGLEN=6.1                                                      225
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   225
C SEPARATOR TUBE->RMIN                                                    226
      MOD4_SPRMIN=0.                                                      226
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 226
C SEPARATOR TUBE->RMAX                                                    227
      MOD4_SPRMAX=0.011                                                   227
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 227
C SEPARATOR TUBE->LENGTH                                                  228
      MOD4_SPLEN=7.8                                                      228
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                228
C WEDGE TUBE: RMIN                                                        229
      MOD4_WGRMIN=0.                                                      229
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     229
C WEDGE TUBE->RMAX                                                        230
      MOD4_WGRMAX=0.15                                                    230
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     230
C WEDGE TUBE->LENGTH                                                      231
      MOD4_WGLEN=10.0                                                     231
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    231
C TOFR FEE PCB THICKNESS                                                  232
      MOD4_FEEH=0.15                                                      232
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 232
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    233
      MOD4_HBWID=0.635                                                    233
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  233
C NUMBER OF GAPS IN MRPC                                                  234
      MOD4_NGAP=6                                                         234
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 234
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            235
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       235
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      235
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    236
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           236
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   236
C *                                                                       236
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    236
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           236
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   236
C *                                                                       237
      CALL AGSBEGM(MODULE,IPRIN)                                          237
      IF (IPRIN.LT.0) RETURN                                              237
C Filling bank MOD5 - RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS       237
      BANK_TITLE='RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          237
      BPATH_MOD5='MOD5*'                                                  237
      IDNUM_MOD5(1)=0                                                     238
C MODULE HEIGHT (R)                                                       238
      MOD5_HEIGHT=1.95                                                    238
      CMOD5_HEIGHT='MODULE HEIGHT (R)'                                    238
C MODULE WIDTH (PHI)                                                      239
      MOD5_WIDTH=21.2                                                     239
      CMOD5_WIDTH='MODULE WIDTH (PHI)'                                    239
C MODULE LENGTH (Z)                                                       240
      MOD5_LENGTH=9.4                                                     240
      CMOD5_LENGTH='MODULE LENGTH (Z)'                                    240
C MODULE DETECTOR CENTER (PHI)                                            241
      MOD5_CENTER=0.35                                                    241
      CMOD5_CENTER='MODULE DETECTOR CENTER (PHI)'                         241
C MRPC ZPOSNS                                                             248
      MOD5_MRPCZ(1)=7.08                                                  248
      MOD5_MRPCZ(2)=13.23                                                 248
      MOD5_MRPCZ(3)=19.23                                                 248
      MOD5_MRPCZ(4)=25.48                                                 248
      MOD5_MRPCZ(5)=33.27                                                 248
      MOD5_MRPCZ(6)=39.48                                                 248
      MOD5_MRPCZ(7)=45.67                                                 248
      MOD5_MRPCZ(8)=51.51                                                 248
      MOD5_MRPCZ(9)=59.66                                                 248
      MOD5_MRPCZ(10)=65.98                                                248
      MOD5_MRPCZ(11)=71.73                                                248
      MOD5_MRPCZ(12)=78.09                                                248
      MOD5_MRPCZ(13)=86.36                                                248
      MOD5_MRPCZ(14)=92.02                                                248
      MOD5_MRPCZ(15)=98.7                                                 248
      MOD5_MRPCZ(16)=104.28                                               248
      MOD5_MRPCZ(17)=112.45                                               248
      MOD5_MRPCZ(18)=118.79                                               248
      MOD5_MRPCZ(19)=125.14                                               248
      MOD5_MRPCZ(20)=131.51                                               248
      MOD5_MRPCZ(21)=139.6                                                248
      MOD5_MRPCZ(22)=145.96                                               248
      MOD5_MRPCZ(23)=152.32                                               248
      MOD5_MRPCZ(24)=158.68                                               248
      MOD5_MRPCZ(25)=167.41                                               248
      MOD5_MRPCZ(26)=173.78                                               248
      MOD5_MRPCZ(27)=180.18                                               248
      MOD5_MRPCZ(28)=186.6                                                248
      MOD5_MRPCZ(29)=195.53                                               248
      MOD5_MRPCZ(30)=202.01                                               248
      MOD5_MRPCZ(31)=208.52                                               248
      MOD5_MRPCZ(32)=215.06                                               248
      CMOD5_MRPCZ='MRPC ZPOSNS'                                           248
C MRPC XPOSNS                                                             254
      MOD5_MRPCX(1)=1.61                                                  254
      MOD5_MRPCX(2)=4.44                                                  254
      MOD5_MRPCX(3)=1.61                                                  254
      MOD5_MRPCX(4)=5.49                                                  254
      MOD5_MRPCX(5)=2.0                                                   254
      MOD5_MRPCX(6)=3.54                                                  254
      MOD5_MRPCX(7)=5.02                                                  254
      MOD5_MRPCX(8)=2.01                                                  254
      MOD5_MRPCX(9)=3.36                                                  254
      MOD5_MRPCX(10)=5.02                                                 254
      MOD5_MRPCX(11)=2.0                                                  254
      MOD5_MRPCX(12)=3.56                                                 254
      MOD5_MRPCX(13)=5.02                                                 254
      MOD5_MRPCX(14)=2.0                                                  254
      MOD5_MRPCX(15)=5.02                                                 254
      MOD5_MRPCX(16)=2.0                                                  254
      MOD5_MRPCX(17)=2.63                                                 254
      MOD5_MRPCX(18)=3.09                                                 254
      MOD5_MRPCX(19)=3.6                                                  254
      MOD5_MRPCX(20)=4.11                                                 254
      MOD5_MRPCX(21)=3.83                                                 254
      MOD5_MRPCX(22)=4.06                                                 254
      MOD5_MRPCX(23)=4.21                                                 254
      MOD5_MRPCX(24)=4.34                                                 254
      MOD5_MRPCX(25)=3.85                                                 254
      MOD5_MRPCX(26)=3.85                                                 254
      MOD5_MRPCX(27)=3.85                                                 254
      MOD5_MRPCX(28)=3.85                                                 254
      MOD5_MRPCX(29)=3.72                                                 254
      MOD5_MRPCX(30)=3.72                                                 254
      MOD5_MRPCX(31)=3.72                                                 254
      MOD5_MRPCX(32)=3.72                                                 254
      CMOD5_MRPCX='MRPC XPOSNS'                                           254
C MRPC ANGLES                                                             258
      MOD5_MRPCA(1)=0.                                                    258
      MOD5_MRPCA(2)=0.                                                    258
      MOD5_MRPCA(3)=0.                                                    258
      MOD5_MRPCA(4)=0.                                                    258
      MOD5_MRPCA(5)=6.                                                    258
      MOD5_MRPCA(6)=6.                                                    258
      MOD5_MRPCA(7)=6.                                                    258
      MOD5_MRPCA(8)=6.                                                    258
      MOD5_MRPCA(9)=6.                                                    258
      MOD5_MRPCA(10)=6.                                                   258
      MOD5_MRPCA(11)=6.                                                   258
      MOD5_MRPCA(12)=6.                                                   258
      MOD5_MRPCA(13)=6.                                                   258
      MOD5_MRPCA(14)=6.                                                   258
      MOD5_MRPCA(15)=6.                                                   258
      MOD5_MRPCA(16)=6.                                                   258
      MOD5_MRPCA(17)=16.                                                  258
      MOD5_MRPCA(18)=16.                                                  258
      MOD5_MRPCA(19)=16.                                                  258
      MOD5_MRPCA(20)=16.                                                  258
      MOD5_MRPCA(21)=19.                                                  258
      MOD5_MRPCA(22)=19.                                                  258
      MOD5_MRPCA(23)=19.                                                  258
      MOD5_MRPCA(24)=19.                                                  258
      MOD5_MRPCA(25)=22.                                                  258
      MOD5_MRPCA(26)=22.                                                  258
      MOD5_MRPCA(27)=22.                                                  258
      MOD5_MRPCA(28)=22.                                                  258
      MOD5_MRPCA(29)=24.                                                  258
      MOD5_MRPCA(30)=24.                                                  258
      MOD5_MRPCA(31)=24.                                                  258
      MOD5_MRPCA(32)=24.                                                  258
      CMOD5_MRPCA='MRPC ANGLES'                                           258
C HC->HEIGHT (R)                                                          259
      MOD5_HCHGT=0.466                                                    259
      CMOD5_HCHGT='HC->HEIGHT (R)'                                        259
C HC->WIDTH (PHI)                                                         260
      MOD5_HCWID=20.8                                                     260
      CMOD5_HCWID='HC->WIDTH (PHI)'                                       260
C HC->LENGTH (Z)                                                          261
      MOD5_HCLEN=8.4                                                      261
      CMOD5_HCLEN='HC->LENGTH (Z)'                                        261
C PCB->HEIGHT (R)                                                         262
      MOD5_PCBHGT=0.15                                                    262
      CMOD5_PCBHGT='PCB->HEIGHT (R)'                                      262
C PCB->WIDTH (PHI)                                                        263
      MOD5_PCBWID=21.0                                                    263
      CMOD5_PCBWID='PCB->WIDTH (PHI)'                                     263
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     264
      MOD5_PCBLEN=9.4                                                     264
      CMOD5_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  264
C MYLAR->HEIGHT                                                           265
      MOD5_MYHGT=0.035                                                    265
      CMOD5_MYHGT='MYLAR->HEIGHT'                                         265
C MYLAR->WIDTH                                                            266
      MOD5_MYWID=21.2                                                     266
      CMOD5_MYWID='MYLAR->WIDTH'                                          266
C MYLAR->LENGTH                                                           267
      MOD5_MYLEN=8.4                                                      267
      CMOD5_MYLEN='MYLAR->LENGTH'                                         267
C GRAPHITE->HEIGHT                                                        268
      MOD5_GRHGT=0.013                                                    268
      CMOD5_GRHGT='GRAPHITE->HEIGHT'                                      268
C GRAPHITE->WIDTH                                                         269
      MOD5_GRWID=20.2                                                     269
      CMOD5_GRWID='GRAPHITE->WIDTH'                                       269
C GRAPHITE->LENGTH                                                        270
      MOD5_GRLEN=7.4                                                      270
      CMOD5_GRLEN='GRAPHITE->LENGTH'                                      270
C OUTER GLASS->HEIGHT                                                     271
      MOD5_OGHGT=0.11                                                     271
      CMOD5_OGHGT='OUTER GLASS->HEIGHT'                                   271
C OUTER GLASS->WIDTH                                                      272
      MOD5_OGWID=20.6                                                     272
      CMOD5_OGWID='OUTER GLASS->WIDTH'                                    272
C OUTER GLASS->LENGTH                                                     273
      MOD5_OGLEN=7.8                                                      273
      CMOD5_OGLEN='OUTER GLASS->LENGTH'                                   273
C INNER GLASS->HEIGHT                                                     274
      MOD5_IGHGT=0.054                                                    274
      CMOD5_IGHGT='INNER GLASS->HEIGHT'                                   274
C INNER GLASS->WIDTH                                                      275
      MOD5_IGWID=20.0                                                     275
      CMOD5_IGWID='INNER GLASS->WIDTH'                                    275
C INNER GLASS->LENGTH                                                     276
      MOD5_IGLEN=6.1                                                      276
      CMOD5_IGLEN='INNER GLASS->LENGTH'                                   276
C SEPARATOR TUBE->RMIN                                                    277
      MOD5_SPRMIN=0.                                                      277
      CMOD5_SPRMIN='SEPARATOR TUBE->RMIN'                                 277
C SEPARATOR TUBE->RMAX                                                    278
      MOD5_SPRMAX=0.011                                                   278
      CMOD5_SPRMAX='SEPARATOR TUBE->RMAX'                                 278
C SEPARATOR TUBE->LENGTH                                                  279
      MOD5_SPLEN=7.8                                                      279
      CMOD5_SPLEN='SEPARATOR TUBE->LENGTH'                                279
C WEDGE TUBE: RMIN                                                        280
      MOD5_WGRMIN=0.                                                      280
      CMOD5_WGRMIN='WEDGE TUBE: RMIN'                                     280
C WEDGE TUBE->RMAX                                                        281
      MOD5_WGRMAX=0.15                                                    281
      CMOD5_WGRMAX='WEDGE TUBE->RMAX'                                     281
C WEDGE TUBE->LENGTH                                                      282
      MOD5_WGLEN=10.0                                                     282
      CMOD5_WGLEN='WEDGE TUBE->LENGTH'                                    282
C TOFR FEE PCB THICKNESS                                                  283
      MOD5_FEEH=0.15                                                      283
      CMOD5_FEEH='TOFR FEE PCB THICKNESS'                                 283
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    284
      MOD5_HBWID=0.635                                                    284
      CMOD5_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  284
C NUMBER OF GAPS IN MRPC                                                  285
      MOD5_NGAP=6                                                         285
      CMOD5_NGAP='NUMBER OF GAPS IN MRPC'                                 285
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ PUTS THIS NEW VALUE   286
      MOD5_TRAYEDGEZ=13.25                                                286
      CMOD5_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ  286
     * PUTS THIS NEW VALUE'                                               286
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    287
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           287
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   287
C *                                                                       287
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    287
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           287
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   287
C use default BTOG bank                                                   288
      BPATH_BTOG='BTOG*'                                                  288
      IDNUM_BTOG(1)=0                                                     288
      CALL AGDGETP('BTOFGEO4','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  288
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      288
     *LINK_BTOG,FLAG_BTOG)                                                288
C use default TRAY bank                                                   289
      BPATH_TRAY='TRAY*'                                                  289
      IDNUM_TRAY(1)=0                                                     289
      CALL AGDGETP('BTOFGEO4','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  289
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      289
     *LINK_TRAY,FLAG_TRAY)                                                289
C use default CTBB bank                                                   290
      BPATH_CTBB='CTBB*'                                                  290
      IDNUM_CTBB(1)=0                                                     290
      CALL AGDGETP('BTOFGEO4','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  290
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      290
     *LINK_CTBB,FLAG_CTBB)                                                290
C use default TOFF bank                                                   291
      BPATH_TOFF='TOFF*'                                                  291
      IDNUM_TOFF(1)=0                                                     291
      CALL AGDGETP('BTOFGEO4','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  291
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      291
     *LINK_TOFF,FLAG_TOFF)                                                291
C use default MODR bank                                                   292
      BPATH_MODR='MODR*'                                                  292
      IDNUM_MODR(1)=0                                                     292
      CALL AGDGETP('BTOFGEO4','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  292
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      292
     *LINK_MODR,FLAG_MODR)                                                292
C use default MOD4 bank                                                   293
      BPATH_MOD4='MOD4*'                                                  293
      IDNUM_MOD4(1)=0                                                     293
      CALL AGDGETP('BTOFGEO4','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  293
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      293
     *LINK_MOD4,FLAG_MOD4)                                                293
C use default MOD5 bank                                                   294
      BPATH_MOD5='MOD5*'                                                  294
      IDNUM_MOD5(1)=0                                                     294
      CALL AGDGETP('BTOFGEO4','*',-999,'MOD5',' ',BANK_MOD5, BPATH_MOD5,  294
     *IDNUM_MOD5,1,MAP_MOD5,NAMES_MOD5,BANK_MOD5,LEN_MOD5,LLEN_MOD5,      294
     *LINK_MOD5,FLAG_MOD5)                                                294
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             295
      CALL AGSRESET                                                       295
      AG_A = 28.08                                                        295
      AG_Z = 14                                                           295
      AG_W = 0.6*1*28./60.                                                295
      AG_TITLE = 'SI'                                                     295
      AG_EXNAME = 'COMPONENT'                                             295
      AG_PARLIST = 'A_Z_W'                                                295
      CALL AXCOMPONENT                                                    295
C *                                                                       295
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  295
C =                                                                       295
      CALL AGSRESET                                                       296
      AG_A = 16                                                           296
      AG_Z = 8                                                            296
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               296
      AG_TITLE = 'O'                                                      296
      AG_EXNAME = 'COMPONENT'                                             296
      AG_PARLIST = 'A_Z_W'                                                296
      CALL AXCOMPONENT                                                    296
C *                                                                       296
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 296
      CALL AGSRESET                                                       297
      AG_A = 12                                                           297
      AG_Z = 6                                                            297
      AG_W = 0.4*8*12./174.                                               297
      AG_TITLE = 'C'                                                      297
      AG_EXNAME = 'COMPONENT'                                             297
      AG_PARLIST = 'A_Z_W'                                                297
      CALL AXCOMPONENT                                                    297
C *                                                                       297
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  297
      CALL AGSRESET                                                       298
      AG_A = 1                                                            298
      AG_Z = 1                                                            298
      AG_W = 0.4*14*1./174.                                               298
      AG_TITLE = 'H'                                                      298
      AG_EXNAME = 'COMPONENT'                                             298
      AG_PARLIST = 'A_Z_W'                                                298
      CALL AXCOMPONENT                                                    298
C *                                                                       298
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  298
      CALL AGSRESET                                                       299
      AG_DENS = 1.7                                                       299
      AG_TITLE = 'G10'                                                    299
      AG_EXNAME = 'MIXTURE'                                               299
      AG_PARLIST = 'DENS'                                                 299
      CALL AXMIXTURE                                                      299
C *                                                                       299
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  299
C ==                                                                      299
      CALL AGSRESET                                                       300
      AG_A = 1                                                            300
      AG_Z = 1                                                            300
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             300
      AG_TITLE = 'H'                                                      300
      AG_EXNAME = 'COMPONENT'                                             300
      AG_PARLIST = 'A_Z_W'                                                300
      CALL AXCOMPONENT                                                    300
C *                                                                       300
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   300
C <===                                                                    300
      CALL AGSRESET                                                       301
      AG_A = 12                                                           301
      AG_Z = 6                                                            301
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            301
      AG_TITLE = 'C'                                                      301
      AG_EXNAME = 'COMPONENT'                                             301
      AG_PARLIST = 'A_Z_W'                                                301
      CALL AXCOMPONENT                                                    301
C *                                                                       301
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  301
C  <===                                                                   301
      CALL AGSRESET                                                       302
      AG_A = 19                                                           302
      AG_Z = 9                                                            302
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           302
      AG_TITLE = 'F'                                                      302
      AG_EXNAME = 'COMPONENT'                                             302
      AG_PARLIST = 'A_Z_W'                                                302
      CALL AXCOMPONENT                                                    302
C *                                                                       302
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         302
      CALL AGSRESET                                                       303
      AG_A = 32                                                           303
      AG_Z = 16                                                           303
      AG_W = 0.+0.05*1*32./146.+0.                                        303
      AG_TITLE = 'S'                                                      303
      AG_EXNAME = 'COMPONENT'                                             303
      AG_PARLIST = 'A_Z_W'                                                303
      CALL AXCOMPONENT                                                    303
C *                                                                       303
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           303
      CALL AGSRESET                                                       304
      AG_DENS = 4.55E-3                                                   304
      AG_TITLE = 'RPCGAS'                                                 304
      AG_EXNAME = 'MIXTURE'                                               304
      AG_PARLIST = 'DENS'                                                 304
      CALL AXMIXTURE                                                      304
C *                                                                       304
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       304
      CALL AGSRESET                                                       305
      AG_A = 27                                                           305
      AG_Z = 13                                                           305
      AG_W = 0.0105                                                       305
      AG_TITLE = 'AL'                                                     305
      AG_EXNAME = 'COMPONENT'                                             305
      AG_PARLIST = 'A_Z_W'                                                305
      CALL AXCOMPONENT                                                    305
C *                                                                       305
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         305
      CALL AGSRESET                                                       306
      AG_A = 14                                                           306
      AG_Z = 7                                                            306
      AG_W = 0.7395                                                       306
      AG_TITLE = 'N'                                                      306
      AG_EXNAME = 'COMPONENT'                                             306
      AG_PARLIST = 'A_Z_W'                                                306
      CALL AXCOMPONENT                                                    306
C *                                                                       306
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 306
      CALL AGSRESET                                                       307
      AG_A = 9                                                            307
      AG_Z = 4.5                                                          307
      AG_W = 0.2500                                                       307
      AG_TITLE = 'ADHESIVE'                                               307
      AG_EXNAME = 'COMPONENT'                                             307
      AG_PARLIST = 'A_Z_W'                                                307
      CALL AXCOMPONENT                                                    307
C *                                                                       307
C Geant MIXTURE : HONEYCOMB ===> DENS=0.282 <===                          307
      CALL AGSRESET                                                       308
      AG_DENS = 0.282                                                     308
      AG_TITLE = 'HONEYCOMB'                                              308
      AG_EXNAME = 'MIXTURE'                                               308
      AG_PARLIST = 'DENS'                                                 308
      CALL AXMIXTURE                                                      308
C *                                                                       308
      CALL AGSBEGM(MODULE,IPRIN)                                          308
C Check IPRIN<0                                                           309
      IF (IPRIN.LT.0) RETURN                                              309
C Create BTOF :                                                           309
C Call To Local Procedure BTOF                                            309
      ASSIGN 5020 TO BLOCK_BTOF                                           309
      GO TO  5010                                                         309
5020  CONTINUE                                                            309
C *                                                                       309
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           310
      CALL AGSRESET                                                       310
      AG_MOTHER = 'CAVE'                                                  310
      AG_TITLE = 'BTOF'                                                   310
      AG_EXNAME = 'POSITION'                                              310
      AG_PARLIST = 'MOTHER'                                               310
      CALL AXPOSITION                                                     310
C *                                                                       310
C ----------------------------------------------------------------------  310
C ------------ BTOF                                                       310
      CALL AGSENDM                                                        312
      RETURN                                                              312
C Begin Local Procedure BTOF                                              312
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   312
5010  CONTINUE                                                            312
         AG_TITLE='BTOF'                                                  312
         AG_MODULE=MODULE                                                 312
         AG_MARK='V00'                                                    312
         CALL AGSPUSH                                                     312
         AG_MODULE=MODULE                                                 312
C    *                                                                    312
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       313
         CALL AGSRESET                                                    313
         AG_SEEN = 1                                                      313
         AG_COLO = 1                                                      313
         AG_TITLE = 'BTOF'                                                313
         AG_EXNAME = 'ATTRIBUTE'                                          313
         AG_PARLIST = 'SEEN_COLO'                                         313
         CALL AXATTRIBUTE                                                 313
C    *                                                                    313
C    Geant MATERIAL : AIR ===> inherited arguments used <===              313
         CALL AGSRESET                                                    314
         AG_TITLE = 'AIR'                                                 314
         AG_EXNAME = 'MATERIAL'                                           314
         AG_PARLIST = 'NONE'                                              314
         CALL AXMATERIAL                                                  314
C    *                                                                    314
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           314
         CALL AGSRESET                                                    315
         AG_TITLE = 'STANDARD'                                            315
         AG_EXNAME = 'MEDIUM'                                             315
         AG_PARLIST = 'NONE'                                              315
         CALL AXMEDIUM                                                    315
C    *                                                                    315
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN RMAX=BTOG_RMAX, DZ=BTOG_DZ <  315
C    ===                                                                  315
         CALL AGSRESET                                                    317
         AG_RMIN = BTOG_RMIN                                              317
         AG_RMAX = BTOG_RMAX                                              317
         AG_DZ = BTOG_DZ                                                  317
         AG_TITLE = 'TUBE'                                                317
         AG_EXNAME = 'SHAPE'                                              317
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      317
         CALL AXSHAPE                                                     317
C    *                                                                    317
         WRITE(LOUT,5030)BTOG_CHOICE,4                                    317
5030     FORMAT('BTOF: btog_choice = ', F4.1, ' itof = ', I2)             319
         YEAR = BTOG_CHOICE - 3                                           320
         WRITE(LOUT,5040)YEAR                                             320
5040     FORMAT(' : this is the run ',I2,' geometry ...')                 322
C    CTB                                                                  323
         CHOICE = 1                                                       323
C    Check (BTOG_CHOICE==2)                                               324
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     324
C    FULL TOFP                                                            325
         CHOICE=BTOG_CHOICE                                               325
         ENDIF                                                            326
C    Check (BTOG_CHOICE==6)                                               327
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     327
C    FULL TOFR                                                            328
         CHOICE=BTOG_CHOICE                                               328
         ENDIF                                                            329
C    Create BTOH :                                                        330
C    Call To Local Procedure BTOH                                         330
         ASSIGN 5060 TO BLOCK_BTOH                                        330
         GO TO  5050                                                      330
5060     CONTINUE                                                         330
C    *                                                                    330
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2 ALPHAY=180 <===              331
         CALL AGSRESET                                                    331
         AG_Z = +BTOG_DZ/2                                                331
         AG_ALPHAN=AG_ALPHAN+1                                            331
         AG_ALPHAK(AG_ALPHAN)=2                                           331
         AG_ALPHAL(AG_ALPHAN)=180                                         331
         AG_TITLE = 'BTOH'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'Z_ALPHAY'                                          331
         CALL AXPOSITION                                                  331
C    *                                                                    331
         CHOICE=BTOG_CHOICE                                               331
C    Create BTOH :                                                        333
C    Call To Local Procedure BTOH                                         333
         ASSIGN 5070 TO BLOCK_BTOH                                        333
         GO TO  5050                                                      333
5070     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2 <===                         334
         CALL AGSRESET                                                    334
         AG_Z = -BTOG_DZ/2                                                334
         AG_TITLE = 'BTOH'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'Z'                                                 334
         CALL AXPOSITION                                                  334
C    *                                                                    334
5011  CALL AGSPOPB                                                        335
      GO TO BLOCK_BTOF, (5010 ,5020)                                      335
C End of Local Procedure                                                  335
C ----------------------------------------------------------------------  336
C ------------ BTOH                                                       336
C Begin Local Procedure BTOH                                              337
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            337
5050  CONTINUE                                                            337
         AG_TITLE='BTOH'                                                  337
         AG_MODULE=MODULE                                                 337
         AG_MARK='V00'                                                    337
         CALL AGSPUSH                                                     337
         AG_MODULE=MODULE                                                 337
C    *                                                                    337
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         338
         CALL AGSRESET                                                    338
         AG_SEEN = 0                                                      338
         AG_COLO = 1                                                      338
         AG_SERIAL = CHOICE                                               338
         AG_TITLE = 'BTOH'                                                338
         AG_EXNAME = 'ATTRIBUTE'                                          338
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  338
         CALL AXATTRIBUTE                                                 338
C    *                                                                    338
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            338
         CALL AGSRESET                                                    339
         AG_DZ = BTOG_DZ/2                                                339
         AG_TITLE = 'TUBE'                                                339
         AG_EXNAME = 'SHAPE'                                              339
         AG_PARLIST = 'DZ'                                                339
         CALL AXSHAPE                                                     339
C    *                                                                    339
C    *                                                                    339
         IS=1                                                             340
            GO TO 5083                                                    340
5081        IS=IS+1                                                       340
5083        IF(IS-(60).GT.0) GO TO 5082                                   340
C       // ALL CTB FOR CHOICE=1                                           341
            TOF=0                                                         341
C       Check (CHOICE==2)                                                 342
            IF ((CHOICE.EQ.2)) THEN                                       342
C       // ALL TOFP                                                       343
            TOF=1                                                         343
            ENDIF                                                         344
C       Check (CHOICE==3&46<=IS&IS<=60)                                   345
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             345
C       // BIG TOFP PATCH, REST CTB                                       346
            TOF=1                                                         346
            ENDIF                                                         347
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              348
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              348
C       // RUN-2 (ONE TOFP TRAY)                                          349
            TOF=1                                                         349
            ENDIF                                                         350
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              351
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              351
C       RUN3ONE TOFP TRAY                                                 352
            TOF=1                                                         352
            ENDIF                                                         353
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 354
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 354
C       AND ONE TOFR TRAY                                                 355
            TOF=2                                                         355
            ENDIF                                                         356
C       Check (CHOICE==6)                                                 357
            IF ((CHOICE.EQ.6)) THEN                                       357
C       // ALL TOFR                                                       358
            TOF=2                                                         358
            ENDIF                                                         359
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              360
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              360
C       RUN4ONE TOFP TRAY MOVED 1 SLOT                                    361
            TOF=1                                                         361
            ENDIF                                                         362
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 363
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 363
C       AND ONE TOFRP TRAY                                                364
            TOF=3                                                         364
            ENDIF                                                         365
C       Check (CHOICE==8&IS==BTOG_POSIT3)                                 366
            IF ((CHOICE.EQ.8.AND.IS.EQ.BTOG_POSIT3)) THEN                 366
C       // RUN-5 (ONE TOFR5 TRAY)                                         367
            TOF=4                                                         367
            ENDIF                                                         368
C       Create BSEC :                                                     369
C       Call To Local Procedure BSEC                                      369
            ASSIGN 5100 TO BLOCK_BSEC                                     369
            GO TO  5090                                                   369
5100        CONTINUE                                                      369
C       *                                                                 369
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   370
            CALL AGSRESET                                                 370
            AG_ALPHAN=AG_ALPHAN+1                                         370
            AG_ALPHAK(AG_ALPHAN)=3                                        370
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 370
            AG_TITLE = 'BSEC'                                             370
            AG_EXNAME = 'POSITION'                                        370
            AG_PARLIST = 'ALPHAZ'                                         370
            CALL AXPOSITION                                               370
C       *                                                                 370
         GO TO 5081                                                       371
5082     CONTINUE                                                         371
5051  CALL AGSPOPB                                                        372
      GO TO BLOCK_BTOH, (5050 ,5060,5070)                                 372
C End of Local Procedure                                                  372
C ----------------------------------------------------------------------  373
C ------------ BSEC                                                       373
C Begin Local Procedure BSEC                                              374
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                374
5090  CONTINUE                                                            374
         AG_TITLE='BSEC'                                                  374
         AG_MODULE=MODULE                                                 374
         AG_MARK='V00'                                                    374
         CALL AGSPUSH                                                     374
         AG_MODULE=MODULE                                                 374
C    *                                                                    374
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            375
         CALL AGSRESET                                                    375
         AG_SEEN = 0                                                      375
         AG_COLO = 1                                                      375
         AG_SERIAL = TOF                                                  375
         AG_TITLE = 'BSEC'                                                375
         AG_EXNAME = 'ATTRIBUTE'                                          375
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  375
         CALL AXATTRIBUTE                                                 375
C    *                                                                    375
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      375
         CALL AGSRESET                                                    376
         AG_PHI1 = -3.0                                                   376
         AG_PHI2 = 3.0                                                    376
         AG_TITLE = 'TUBS'                                                376
         AG_EXNAME = 'SHAPE'                                              376
         AG_PARLIST = 'PHI1_PHI2'                                         376
         CALL AXSHAPE                                                     376
C    *                                                                    376
C    Create BTRA :                                                        377
C    Call To Local Procedure BTRA                                         377
         ASSIGN 5120 TO BLOCK_BTRA                                        377
         GO TO  5110                                                      377
5120     CONTINUE                                                         377
C    *                                                                    377
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  378
C    RAY_STRIPT)/2 <===                                                   378
         CALL AGSRESET                                                    378
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2       378
         AG_TITLE = 'BTRA'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'X'                                                 378
         CALL AXPOSITION                                                  378
C    *                                                                    378
C    Check TOF==2                                                         378
         IF (TOF.EQ.2) THEN                                               379
C    Create BRFE :                                                        380
C    Call To Local Procedure BRFE                                         380
         ASSIGN 5140 TO BLOCK_BRFE                                        380
         GO TO  5130                                                      380
5140     CONTINUE                                                         380
C    *                                                                    380
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  382
C    AY_HEIGHT+MODR_FEEH/2 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===                 382
         CALL AGSRESET                                                    382
         AG_X =                                                           382
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/2      382
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   382
         AG_TITLE = 'BRFE'                                                382
         AG_EXNAME = 'POSITION'                                           382
         AG_PARLIST = 'X_Z'                                               382
         CALL AXPOSITION                                                  382
C    *                                                                    382
         ENDIF                                                            382
5091  CALL AGSPOPB                                                        384
      GO TO BLOCK_BSEC, (5090 ,5100)                                      384
C End of Local Procedure                                                  384
C ----------------------------------------------------------------------  385
C ------------ BTRA                                                       385
C Begin Local Procedure BTRA                                              386
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             386
5110  CONTINUE                                                            386
         AG_TITLE='BTRA'                                                  386
         AG_MODULE=MODULE                                                 386
         AG_MARK='V00'                                                    386
         CALL AGSPUSH                                                     386
         AG_MODULE=MODULE                                                 386
C    *                                                                    386
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       387
         CALL AGSRESET                                                    387
         AG_SEEN = 1                                                      387
         AG_COLO = 2                                                      387
         AG_TITLE = 'BTRA'                                                387
         AG_EXNAME = 'ATTRIBUTE'                                          387
         AG_PARLIST = 'SEEN_COLO'                                         387
         CALL AXATTRIBUTE                                                 387
C    *                                                                    387
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  387
C    , DY=TRAY_WIDTH/2 <===                                               387
         CALL AGSRESET                                                    389
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                389
         AG_DY = TRAY_WIDTH/2                                             389
         AG_TITLE = 'BOX'                                                 389
         AG_EXNAME = 'SHAPE'                                              389
         AG_PARLIST = 'DX_DY'                                             389
         CALL AXSHAPE                                                     389
C    *                                                                    389
C    Create BXTR :                                                        390
C    Call To Local Procedure BXTR                                         390
         ASSIGN 5160 TO BLOCK_BXTR                                        390
         GO TO  5150                                                      390
5160     CONTINUE                                                         390
C    *                                                                    390
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  391
C    DZ-TRAY_LENGTH)/2 <===                                               391
         CALL AGSRESET                                                    391
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             391
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   391
         AG_TITLE = 'BXTR'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'X_Z'                                               391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Create BUND :                                                        392
C    Call To Local Procedure BUND                                         392
         ASSIGN 5180 TO BLOCK_BUND                                        392
         GO TO  5170                                                      392
5180     CONTINUE                                                         392
C    *                                                                    392
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  393
C    Z-TRAY_SUPLEN)/2 <===                                                393
         CALL AGSRESET                                                    393
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              393
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   393
         AG_TITLE = 'BUND'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'X_Z'                                               393
         CALL AXPOSITION                                                  393
C    *                                                                    393
5111  CALL AGSPOPB                                                        394
      GO TO BLOCK_BTRA, (5110 ,5120)                                      394
C End of Local Procedure                                                  394
C ----------------------------------------------------------------------  395
C ------------ BRFE                                                       395
C Begin Local Procedure BRFE                                              396
C BRFE IS THE FEE OF TOFR (RUN-3)                                         396
5130  CONTINUE                                                            396
         AG_TITLE='BRFE'                                                  396
         AG_MODULE=MODULE                                                 396
         AG_MARK='V00'                                                    396
         CALL AGSPUSH                                                     396
         AG_MODULE=MODULE                                                 396
C    *                                                                    396
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       397
         CALL AGSRESET                                                    397
         AG_SEEN = 0                                                      397
         AG_COLO = 3                                                      397
         AG_TITLE = 'BRFE'                                                397
         AG_EXNAME = 'ATTRIBUTE'                                          397
         AG_PARLIST = 'SEEN_COLO'                                         397
         CALL AXATTRIBUTE                                                 397
C    *                                                                    397
C    Geant MATERIAL : G10 ===> inherited arguments used <===              397
         CALL AGSRESET                                                    398
         AG_TITLE = 'G10'                                                 398
         AG_EXNAME = 'MATERIAL'                                           398
         AG_PARLIST = 'NONE'                                              398
         CALL AXMATERIAL                                                  398
C    *                                                                    398
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  398
C    TH/2 <===                                                            398
         CALL AGSRESET                                                    400
         AG_DX = MODR_FEEH/2                                              400
         AG_DY = TRAY_WIDTH/2                                             400
         AG_DZ = TRAY_LENGTH/2                                            400
         AG_TITLE = 'BOX'                                                 400
         AG_EXNAME = 'SHAPE'                                              400
         AG_PARLIST = 'DX_DY_DZ'                                          400
         CALL AXSHAPE                                                     400
C    *                                                                    400
5131  CALL AGSPOPB                                                        401
      GO TO BLOCK_BRFE, (5130 ,5140)                                      401
C End of Local Procedure                                                  401
C ----------------------------------------------------------------------  402
C ------------ BXTR                                                       402
C Begin Local Procedure BXTR                                              403
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         403
5150  CONTINUE                                                            403
         AG_TITLE='BXTR'                                                  403
         AG_MODULE=MODULE                                                 403
         AG_MARK='V00'                                                    403
         CALL AGSPUSH                                                     403
         AG_MODULE=MODULE                                                 403
C    *                                                                    403
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       404
         CALL AGSRESET                                                    404
         AG_SEEN = 0                                                      404
         AG_COLO = 2                                                      404
         AG_TITLE = 'BXTR'                                                404
         AG_EXNAME = 'ATTRIBUTE'                                          404
         AG_PARLIST = 'SEEN_COLO'                                         404
         CALL AXATTRIBUTE                                                 404
C    *                                                                    404
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        404
         CALL AGSRESET                                                    405
         AG_TITLE = 'ALUMINIUM'                                           405
         AG_EXNAME = 'MATERIAL'                                           405
         AG_PARLIST = 'NONE'                                              405
         CALL AXMATERIAL                                                  405
C    *                                                                    405
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        405
         CALL AGSRESET                                                    406
         AG_DX = TRAY_HEIGHT/2                                            406
         AG_DZ = TRAY_LENGTH/2                                            406
         AG_TITLE = 'BOX'                                                 406
         AG_EXNAME = 'SHAPE'                                              406
         AG_PARLIST = 'DX_DZ'                                             406
         CALL AXSHAPE                                                     406
C    *                                                                    406
C    Check TOF==1                                                         406
         IF (TOF.EQ.1) THEN                                               407
C    Create BTTC :                                                        408
C    Call To Local Procedure BTTC                                         408
         ASSIGN 5200 TO BLOCK_BTTC                                        408
         GO TO  5190                                                      408
5200     CONTINUE                                                         408
C    *                                                                    408
C    Geant POSITION : BTTC ===> inherited arguments used <===             409
         CALL AGSRESET                                                    409
         AG_TITLE = 'BTTC'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'NONE'                                              409
         CALL AXPOSITION                                                  409
C    *                                                                    409
         ELSEIF (TOF.GE.2) THEN                                           409
C    Create BRTC :                                                        411
C    Call To Local Procedure BRTC                                         411
         ASSIGN 5220 TO BLOCK_BRTC                                        411
         GO TO  5210                                                      411
5220     CONTINUE                                                         411
C    *                                                                    411
C    Geant POSITION : BRTC ===> inherited arguments used <===             412
         CALL AGSRESET                                                    412
         AG_TITLE = 'BRTC'                                                412
         AG_EXNAME = 'POSITION'                                           412
         AG_PARLIST = 'NONE'                                              412
         CALL AXPOSITION                                                  412
C    *                                                                    412
C    Create BUPC :                                                        413
C    Call To Local Procedure BUPC                                         413
         ASSIGN 5240 TO BLOCK_BUPC                                        413
         GO TO  5230                                                      413
5240     CONTINUE                                                         413
C    *                                                                    413
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_WALLTHK)/2 <===       414
         CALL AGSRESET                                                    414
         AG_X = (TRAY_HEIGHT-TRAY_WALLTHK)/2                              414
         AG_TITLE = 'BUPC'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'X'                                                 414
         CALL AXPOSITION                                                  414
C    *                                                                    414
         ELSE                                                             414
C    Create BMTC :                                                        416
C    Call To Local Procedure BMTC                                         416
         ASSIGN 5260 TO BLOCK_BMTC                                        416
         GO TO  5250                                                      416
5260     CONTINUE                                                         416
C    *                                                                    416
C    Geant POSITION : BMTC ===> inherited arguments used <===             417
         CALL AGSRESET                                                    417
         AG_TITLE = 'BMTC'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'NONE'                                              417
         CALL AXPOSITION                                                  417
C    *                                                                    417
         ENDIF                                                            417
5151  CALL AGSPOPB                                                        419
      GO TO BLOCK_BXTR, (5150 ,5160)                                      419
C End of Local Procedure                                                  419
C ----------------------------------------------------------------------  420
C ------------ BMTC                                                       420
C Begin Local Procedure BMTC                                              421
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            421
5250  CONTINUE                                                            421
         AG_TITLE='BMTC'                                                  421
         AG_MODULE=MODULE                                                 421
         AG_MARK='V00'                                                    421
         CALL AGSPUSH                                                     421
         AG_MODULE=MODULE                                                 421
C    *                                                                    421
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      422
         CALL AGSRESET                                                    422
         AG_SEEN = -1                                                     422
         AG_COLO = 5                                                      422
         AG_TITLE = 'BMTC'                                                422
         AG_EXNAME = 'ATTRIBUTE'                                          422
         AG_PARLIST = 'SEEN_COLO'                                         422
         CALL AXATTRIBUTE                                                 422
C    *                                                                    422
C    Geant MATERIAL : AIR ===> inherited arguments used <===              422
         CALL AGSRESET                                                    423
         AG_TITLE = 'AIR'                                                 423
         AG_EXNAME = 'MATERIAL'                                           423
         AG_PARLIST = 'NONE'                                              423
         CALL AXMATERIAL                                                  423
C    *                                                                    423
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  423
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   423
         CALL AGSRESET                                                    425
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               425
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                425
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               425
         AG_TITLE = 'BOX'                                                 425
         AG_EXNAME = 'SHAPE'                                              425
         AG_PARLIST = 'DX_DY_DZ'                                          425
         CALL AXSHAPE                                                     425
C    *                                                                    425
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           425
     *   CTBB_WRAP                                                        425
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             427
C    Create BXSA :                                                        428
C    Call To Local Procedure BXSA                                         428
         ASSIGN 5280 TO BLOCK_BXSA                                        428
         GO TO  5270                                                      428
5280     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  430
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   430
         CALL AGSRESET                                                    430
         AG_X = XPOS                                                      430
         AG_Z = ZPOS                                                      430
         AG_DZ = CTBB_SLAB1LEN/2                                          430
         AG_DX = CTBB_SLABTHCK/2                                          430
         AG_DY = CTBB_SLABWID/2                                           430
         AG_TITLE = 'BXSA'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      430
         CALL AXPOSITION                                                  430
C    *                                                                    430
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   430
C    Create BCCV :                                                        432
C    Call To Local Procedure BCCV                                         432
         ASSIGN 5300 TO BLOCK_BCCV                                        432
         GO TO  5290                                                      432
5300     CONTINUE                                                         432
C    *                                                                    432
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  434
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  434
C    K/2 <===                                                             434
         CALL AGSRESET                                                    434
         AG_X = XPOS                                                      434
         AG_Z = ZPOS                                                      434
         AG_DX2 = CTBB_SLABTHCK/2                                         434
         AG_DY2 = CTBB_SLABWID/2                                          434
         AG_DY1 = CTBB_CONVWIDM/2                                         434
         AG_DZ = CTBB_CONVLEN/2                                           434
         AG_DX1 = CTBB_SLABTHCK/2                                         434
         AG_TITLE = 'BCCV'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            434
         CALL AXPOSITION                                                  434
C    *                                                                    434
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     434
C    Create BCPM :                                                        436
C    Call To Local Procedure BCPM                                         436
         ASSIGN 5320 TO BLOCK_BCPM                                        436
         GO TO  5310                                                      436
5320     CONTINUE                                                         436
C    *                                                                    436
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  438
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      438
         CALL AGSRESET                                                    438
         AG_X = XPOS                                                      438
         AG_Z = ZPOS                                                      438
         AG_RMIN = CTBB_PMTMINR                                           438
         AG_RMAX = CTBB_PMTMAXR                                           438
         AG_DZ = CTBB_PMTLEN/2                                            438
         AG_TITLE = 'BCPM'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  438
         CALL AXPOSITION                                                  438
C    *                                                                    438
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     438
C    Create BCSK :                                                        440
C    Call To Local Procedure BCSK                                         440
         ASSIGN 5340 TO BLOCK_BCSK                                        440
         GO TO  5330                                                      440
5340     CONTINUE                                                         440
C    *                                                                    440
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  442
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   442
         CALL AGSRESET                                                    442
         AG_X = XPOS                                                      442
         AG_Z = ZPOS                                                      442
         AG_RMIN = CTBB_BASEMINR                                          442
         AG_RMAX = CTBB_BASEMAXR                                          442
         AG_DZ = CTBB_BASELEN/2                                           442
         AG_TITLE = 'BCSK'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  442
         CALL AXPOSITION                                                  442
C    *                                                                    442
C    Create BZEL :                                                        443
C    Call To Local Procedure BZEL                                         443
         ASSIGN 5360 TO BLOCK_BZEL                                        443
         GO TO  5350                                                      443
5360     CONTINUE                                                         443
C    *                                                                    443
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  445
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 445
         CALL AGSRESET                                                    445
         AG_X = XPOS                                                      445
         AG_Z = ZPOS                                                      445
         AG_DZ = CTBB_BASELEN/2                                           445
         AG_DX = CTBB_ELECTHCK/2                                          445
         AG_DY = CTBB_BASEMINR-0.1                                        445
         AG_TITLE = 'BZEL'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      445
         CALL AXPOSITION                                                  445
C    *                                                                    445
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           445
     *   CTBB_WRAP - CTBB_SHIM                                            445
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             447
C    Create BXSA :                                                        448
C    Call To Local Procedure BXSA                                         448
         ASSIGN 5370 TO BLOCK_BXSA                                        448
         GO TO  5270                                                      448
5370     CONTINUE                                                         448
C    *                                                                    448
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  450
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  450
         CALL AGSRESET                                                    450
         AG_X = XPOS                                                      450
         AG_Z = -ZPOS                                                     450
         AG_DZ = CTBB_SLAB2LEN/2                                          450
         AG_DX = CTBB_SLABTHCK/2                                          450
         AG_DY = CTBB_SLABWID/2                                           450
         AG_TITLE = 'BXSA'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      450
         CALL AXPOSITION                                                  450
C    *                                                                    450
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   450
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  453
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  453
C    CK/2 ALPHAX=180 <===                                                 453
         CALL AGSRESET                                                    453
         AG_X = XPOS                                                      453
         AG_Z = -ZPOS                                                     453
         AG_DX2 = CTBB_SLABTHCK/2                                         453
         AG_DY2 = CTBB_SLABWID/2                                          453
         AG_DY1 = CTBB_CONVWIDM/2                                         453
         AG_DZ = CTBB_CONVLEN/2                                           453
         AG_DX1 = CTBB_SLABTHCK/2                                         453
         AG_ALPHAN=AG_ALPHAN+1                                            453
         AG_ALPHAK(AG_ALPHAN)=1                                           453
         AG_ALPHAL(AG_ALPHAN)=180                                         453
         AG_TITLE = 'BCCV'                                                453
         AG_EXNAME = 'POSITION'                                           453
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     453
         CALL AXPOSITION                                                  453
C    *                                                                    453
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     453
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  456
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     456
         CALL AGSRESET                                                    456
         AG_X = XPOS                                                      456
         AG_Z = -ZPOS                                                     456
         AG_RMIN = CTBB_PMTMINR                                           456
         AG_RMAX = CTBB_PMTMAXR                                           456
         AG_DZ = CTBB_PMTLEN/2                                            456
         AG_TITLE = 'BCPM'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  456
         CALL AXPOSITION                                                  456
C    *                                                                    456
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     456
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  459
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  459
         CALL AGSRESET                                                    459
         AG_X = XPOS                                                      459
         AG_Z = -ZPOS                                                     459
         AG_RMIN = CTBB_BASEMINR                                          459
         AG_RMAX = CTBB_BASEMAXR                                          459
         AG_DZ = CTBB_BASELEN/2                                           459
         AG_TITLE = 'BCSK'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  459
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                459
         CALL AGSRESET                                                    461
         AG_X = XPOS                                                      461
         AG_Z = -ZPOS                                                     461
         AG_DZ = CTBB_BASELEN/2                                           461
         AG_DX = CTBB_ELECTHCK/2                                          461
         AG_DY = CTBB_BASEMINR-0.1                                        461
         AG_TITLE = 'BZEL'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      461
         CALL AXPOSITION                                                  461
C    *                                                                    461
5251  CALL AGSPOPB                                                        462
      GO TO BLOCK_BMTC, (5250 ,5260)                                      462
C End of Local Procedure                                                  462
C ----------------------------------------------------------------------  463
C ------------ BTTC                                                       463
C Begin Local Procedure BTTC                                              464
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           464
5190  CONTINUE                                                            464
         AG_TITLE='BTTC'                                                  464
         AG_MODULE=MODULE                                                 464
         AG_MARK='V00'                                                    464
         CALL AGSPUSH                                                     464
         AG_MODULE=MODULE                                                 464
C    *                                                                    464
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       465
         CALL AGSRESET                                                    465
         AG_SEEN = 0                                                      465
         AG_COLO = 3                                                      465
         AG_TITLE = 'BTTC'                                                465
         AG_EXNAME = 'ATTRIBUTE'                                          465
         AG_PARLIST = 'SEEN_COLO'                                         465
         CALL AXATTRIBUTE                                                 465
C    *                                                                    465
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           465
         CALL AGSRESET                                                    466
         AG_A = 12                                                        466
         AG_Z = 6                                                         466
         AG_W = 1                                                         466
         AG_TITLE = 'C'                                                   466
         AG_EXNAME = 'COMPONENT'                                          466
         AG_PARLIST = 'A_Z_W'                                             466
         CALL AXCOMPONENT                                                 466
C    *                                                                    466
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           466
         CALL AGSRESET                                                    467
         AG_A = 1                                                         467
         AG_Z = 1                                                         467
         AG_W = 2                                                         467
         AG_TITLE = 'H2'                                                  467
         AG_EXNAME = 'COMPONENT'                                          467
         AG_PARLIST = 'A_Z_W'                                             467
         CALL AXCOMPONENT                                                 467
C    *                                                                    467
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       467
         CALL AGSRESET                                                    468
         AG_DENS = 0.048                                                  468
         AG_TITLE = 'LASTAFOAM'                                           468
         AG_EXNAME = 'MIXTURE'                                            468
         AG_PARLIST = 'DENS'                                              468
         CALL AXMIXTURE                                                   468
C    *                                                                    468
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  468
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   468
         CALL AGSRESET                                                    470
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               470
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                470
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               470
         AG_TITLE = 'BOX'                                                 470
         AG_EXNAME = 'SHAPE'                                              470
         AG_PARLIST = 'DX_DY_DZ'                                          470
         CALL AXSHAPE                                                     470
C    *                                                                    470
         SUBLEN =((TOFF_SLAT02Z + 15.5) - (TOFF_SLAT10Z - 15.5))          470
         SUBCEN =(TOFF_SLAT02Z + 15.5) - SUBLEN/2.                        472
         IWID=4                                                           473
C    Create BMAA :                                                        474
C    Call To Local Procedure BMAA                                         474
         ASSIGN 5390 TO BLOCK_BMAA                                        474
         GO TO  5380                                                      474
5390     CONTINUE                                                         474
C    *                                                                    474
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            475
         CALL AGSRESET                                                    475
         AG_X = 0                                                         475
         AG_Z = SUBCEN                                                    475
         AG_KONLY = 'MANY'                                                475
         AG_TITLE = 'BMAA'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'X_Z_KONLY'                                         475
         CALL AXPOSITION                                                  475
C    *                                                                    475
         IWID=5                                                           475
C    Create BMAA :                                                        477
C    Call To Local Procedure BMAA                                         477
         ASSIGN 5400 TO BLOCK_BMAA                                        477
         GO TO  5380                                                      477
5400     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLAT01Z KONLY='MANY' <===    478
         CALL AGSRESET                                                    478
         AG_X = 0.0                                                       478
         AG_Z = TOFF_SLAT01Z                                              478
         AG_KONLY = 'MANY'                                                478
         AG_TITLE = 'BMAA'                                                478
         AG_EXNAME = 'POSITION'                                           478
         AG_PARLIST = 'X_Z_KONLY'                                         478
         CALL AXPOSITION                                                  478
C    *                                                                    478
C    Create BCOO :                                                        479
C    Call To Local Procedure BCOO                                         479
         ASSIGN 5420 TO BLOCK_BCOO                                        479
         GO TO  5410                                                      479
5420     CONTINUE                                                         479
C    *                                                                    479
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               480
         CALL AGSRESET                                                    480
         AG_X = 0                                                         480
         AG_Y = 0                                                         480
         AG_DZ = 0                                                        480
         AG_DX = 0                                                        480
         AG_DY = 0                                                        480
         AG_TITLE = 'BCOO'                                                480
         AG_EXNAME = 'POSITION'                                           480
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      480
         CALL AXPOSITION                                                  480
C    *                                                                    480
         AG_DZ = TOFF_ELECLEN/2                                           480
         AG_DX = TOFF_ELECTHCK/2                                          481
         AG_DY = TOFF_ELECWID/2                                           481
C    Create BFEE :                                                        481
C    Call To Local Procedure BFEE                                         481
         ASSIGN 5440 TO BLOCK_BFEE                                        481
         GO TO  5430                                                      481
5440     CONTINUE                                                         481
C    *                                                                    481
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC01Z-TOFF_ELECL  482
C    EN/2 <===                                                            482
         CALL AGSRESET                                                    482
         AG_X = TOFF_ELECHGT                                              482
         AG_Z = TOFF_ELEC01Z-TOFF_ELECLEN/2                               482
         AG_TITLE = 'BFEE'                                                482
         AG_EXNAME = 'POSITION'                                           482
         AG_PARLIST = 'X_Z'                                               482
         CALL AXPOSITION                                                  482
C    *                                                                    482
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC02Z-TOFF_ELECL  482
C    EN/2 <===                                                            482
         CALL AGSRESET                                                    483
         AG_X = TOFF_ELECHGT                                              483
         AG_Z = TOFF_ELEC02Z-TOFF_ELECLEN/2                               483
         AG_TITLE = 'BFEE'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Z'                                               483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC03Z-TOFF_ELECL  483
C    EN/2 <===                                                            483
         CALL AGSRESET                                                    484
         AG_X = TOFF_ELECHGT                                              484
         AG_Z = TOFF_ELEC03Z-TOFF_ELECLEN/2                               484
         AG_TITLE = 'BFEE'                                                484
         AG_EXNAME = 'POSITION'                                           484
         AG_PARLIST = 'X_Z'                                               484
         CALL AXPOSITION                                                  484
C    *                                                                    484
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC04Z-TOFF_ELECL  484
C    EN/2 <===                                                            484
         CALL AGSRESET                                                    485
         AG_X = TOFF_ELECHGT                                              485
         AG_Z = TOFF_ELEC04Z-TOFF_ELECLEN/2                               485
         AG_TITLE = 'BFEE'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'X_Z'                                               485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC05Z-TOFF_ELECL  485
C    EN/2 <===                                                            485
         CALL AGSRESET                                                    486
         AG_X = TOFF_ELECHGT                                              486
         AG_Z = TOFF_ELEC05Z-TOFF_ELECLEN/2                               486
         AG_TITLE = 'BFEE'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'X_Z'                                               486
         CALL AXPOSITION                                                  486
C    *                                                                    486
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC06Z-TOFF_ELECL  486
C    EN/2 <===                                                            486
         CALL AGSRESET                                                    487
         AG_X = TOFF_ELECHGT                                              487
         AG_Z = TOFF_ELEC06Z-TOFF_ELECLEN/2                               487
         AG_TITLE = 'BFEE'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'X_Z'                                               487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC07Z-TOFF_ELECL  487
C    EN/2 <===                                                            487
         CALL AGSRESET                                                    488
         AG_X = TOFF_ELECHGT                                              488
         AG_Z = TOFF_ELEC07Z-TOFF_ELECLEN/2                               488
         AG_TITLE = 'BFEE'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Z'                                               488
         CALL AXPOSITION                                                  488
C    *                                                                    488
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC08Z-TOFF_ELECL  488
C    EN/2 <===                                                            488
         CALL AGSRESET                                                    489
         AG_X = TOFF_ELECHGT                                              489
         AG_Z = TOFF_ELEC08Z-TOFF_ELECLEN/2                               489
         AG_TITLE = 'BFEE'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'X_Z'                                               489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC09Z-TOFF_ELECL  489
C    EN/2 <===                                                            489
         CALL AGSRESET                                                    490
         AG_X = TOFF_ELECHGT                                              490
         AG_Z = TOFF_ELEC09Z-TOFF_ELECLEN/2                               490
         AG_TITLE = 'BFEE'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Z'                                               490
         CALL AXPOSITION                                                  490
C    *                                                                    490
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC10Z-TOFF_ELECL  490
C    EN/2 <===                                                            490
         CALL AGSRESET                                                    491
         AG_X = TOFF_ELECHGT                                              491
         AG_Z = TOFF_ELEC10Z-TOFF_ELECLEN/2                               491
         AG_TITLE = 'BFEE'                                                491
         AG_EXNAME = 'POSITION'                                           491
         AG_PARLIST = 'X_Z'                                               491
         CALL AXPOSITION                                                  491
C    *                                                                    491
C    Create BPLA :                                                        492
C    Call To Local Procedure BPLA                                         492
         ASSIGN 5460 TO BLOCK_BPLA                                        492
         GO TO  5450                                                      492
5460     CONTINUE                                                         492
C    *                                                                    492
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC01Z+3.0 <=  493
C    ==                                                                   493
         CALL AGSRESET                                                    493
         AG_X = TOFF_ELECHGT                                              493
         AG_Y = 0                                                         493
         AG_Z = TOFF_ELEC01Z+3.0                                          493
         AG_TITLE = 'BPLA'                                                493
         AG_EXNAME = 'POSITION'                                           493
         AG_PARLIST = 'X_Y_Z'                                             493
         CALL AXPOSITION                                                  493
C    *                                                                    493
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC02Z+3.0 <=  493
C    ==                                                                   493
         CALL AGSRESET                                                    494
         AG_X = TOFF_ELECHGT                                              494
         AG_Y = 0                                                         494
         AG_Z = TOFF_ELEC02Z+3.0                                          494
         AG_TITLE = 'BPLA'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'X_Y_Z'                                             494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC03Z+3.0 <=  494
C    ==                                                                   494
         CALL AGSRESET                                                    495
         AG_X = TOFF_ELECHGT                                              495
         AG_Y = 0                                                         495
         AG_Z = TOFF_ELEC03Z+3.0                                          495
         AG_TITLE = 'BPLA'                                                495
         AG_EXNAME = 'POSITION'                                           495
         AG_PARLIST = 'X_Y_Z'                                             495
         CALL AXPOSITION                                                  495
C    *                                                                    495
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC04Z+3.0 <=  495
C    ==                                                                   495
         CALL AGSRESET                                                    496
         AG_X = TOFF_ELECHGT                                              496
         AG_Y = 0                                                         496
         AG_Z = TOFF_ELEC04Z+3.0                                          496
         AG_TITLE = 'BPLA'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'X_Y_Z'                                             496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC05Z+3.0 <=  496
C    ==                                                                   496
         CALL AGSRESET                                                    497
         AG_X = TOFF_ELECHGT                                              497
         AG_Y = 0                                                         497
         AG_Z = TOFF_ELEC05Z+3.0                                          497
         AG_TITLE = 'BPLA'                                                497
         AG_EXNAME = 'POSITION'                                           497
         AG_PARLIST = 'X_Y_Z'                                             497
         CALL AXPOSITION                                                  497
C    *                                                                    497
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC06Z+3.0 <=  497
C    ==                                                                   497
         CALL AGSRESET                                                    498
         AG_X = TOFF_ELECHGT                                              498
         AG_Y = 0                                                         498
         AG_Z = TOFF_ELEC06Z+3.0                                          498
         AG_TITLE = 'BPLA'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'X_Y_Z'                                             498
         CALL AXPOSITION                                                  498
C    *                                                                    498
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC07Z+3.0 <=  498
C    ==                                                                   498
         CALL AGSRESET                                                    499
         AG_X = TOFF_ELECHGT                                              499
         AG_Y = 0                                                         499
         AG_Z = TOFF_ELEC07Z+3.0                                          499
         AG_TITLE = 'BPLA'                                                499
         AG_EXNAME = 'POSITION'                                           499
         AG_PARLIST = 'X_Y_Z'                                             499
         CALL AXPOSITION                                                  499
C    *                                                                    499
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC08Z+3.0 <=  499
C    ==                                                                   499
         CALL AGSRESET                                                    500
         AG_X = TOFF_ELECHGT                                              500
         AG_Y = 0                                                         500
         AG_Z = TOFF_ELEC08Z+3.0                                          500
         AG_TITLE = 'BPLA'                                                500
         AG_EXNAME = 'POSITION'                                           500
         AG_PARLIST = 'X_Y_Z'                                             500
         CALL AXPOSITION                                                  500
C    *                                                                    500
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC09Z+3.0 <=  500
C    ==                                                                   500
         CALL AGSRESET                                                    501
         AG_X = TOFF_ELECHGT                                              501
         AG_Y = 0                                                         501
         AG_Z = TOFF_ELEC09Z+3.0                                          501
         AG_TITLE = 'BPLA'                                                501
         AG_EXNAME = 'POSITION'                                           501
         AG_PARLIST = 'X_Y_Z'                                             501
         CALL AXPOSITION                                                  501
C    *                                                                    501
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC10Z+3.0 <=  501
C    ==                                                                   501
         CALL AGSRESET                                                    502
         AG_X = TOFF_ELECHGT                                              502
         AG_Y = 0                                                         502
         AG_Z = TOFF_ELEC10Z+3.0                                          502
         AG_TITLE = 'BPLA'                                                502
         AG_EXNAME = 'POSITION'                                           502
         AG_PARLIST = 'X_Y_Z'                                             502
         CALL AXPOSITION                                                  502
C    *                                                                    502
5191  CALL AGSPOPB                                                        503
      GO TO BLOCK_BTTC, (5190 ,5200)                                      503
C End of Local Procedure                                                  503
C ----------------------------------------------------------------------  504
C ------------ BUPC                                                       504
C Begin Local Procedure BUPC                                              505
C BUPC IS THE UP PCB COVER OF TOFR                                        505
5230  CONTINUE                                                            505
         AG_TITLE='BUPC'                                                  505
         AG_MODULE=MODULE                                                 505
         AG_MARK='V00'                                                    505
         CALL AGSPUSH                                                     505
         AG_MODULE=MODULE                                                 505
C    *                                                                    505
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       506
         CALL AGSRESET                                                    506
         AG_SEEN = 0                                                      506
         AG_COLO = 3                                                      506
         AG_TITLE = 'BUPC'                                                506
         AG_EXNAME = 'ATTRIBUTE'                                          506
         AG_PARLIST = 'SEEN_COLO'                                         506
         CALL AXATTRIBUTE                                                 506
C    *                                                                    506
C    Geant MATERIAL : G10 ===> inherited arguments used <===              506
         CALL AGSRESET                                                    507
         AG_TITLE = 'G10'                                                 507
         AG_EXNAME = 'MATERIAL'                                           507
         AG_PARLIST = 'NONE'                                              507
         CALL AXMATERIAL                                                  507
C    *                                                                    507
C    Geant SHAPE : BOX ===> DX=TRAY_WALLTHK/2 DY=TRAY_WIDTH/2-TRAY_WALLT  507
C    HK, DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                               507
         CALL AGSRESET                                                    509
         AG_DX = TRAY_WALLTHK/2                                           509
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                509
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               509
         AG_TITLE = 'BOX'                                                 509
         AG_EXNAME = 'SHAPE'                                              509
         AG_PARLIST = 'DX_DY_DZ'                                          509
         CALL AXSHAPE                                                     509
C    *                                                                    509
5231  CALL AGSPOPB                                                        510
      GO TO BLOCK_BUPC, (5230 ,5240)                                      510
C End of Local Procedure                                                  510
C ----------------------------------------------------------------------  511
C ------------ BRTC                                                       511
C Begin Local Procedure BRTC                                              513
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  513
C R RUN4)                                                                 513
5210  CONTINUE                                                            513
         AG_TITLE='BRTC'                                                  513
         AG_MODULE=MODULE                                                 513
         AG_MARK='V00'                                                    513
         CALL AGSPUSH                                                     513
         AG_MODULE=MODULE                                                 513
C    *                                                                    513
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       514
         CALL AGSRESET                                                    514
         AG_SEEN = 0                                                      514
         AG_COLO = 5                                                      514
         AG_TITLE = 'BRTC'                                                514
         AG_EXNAME = 'ATTRIBUTE'                                          514
         AG_PARLIST = 'SEEN_COLO'                                         514
         CALL AXATTRIBUTE                                                 514
C    *                                                                    514
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        514
         CALL AGSRESET                                                    515
         AG_TITLE = 'HONEYCOMB'                                           515
         AG_EXNAME = 'MATERIAL'                                           515
         AG_PARLIST = 'NONE'                                              515
         CALL AXMATERIAL                                                  515
C    *                                                                    515
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  515
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   515
         CALL AGSRESET                                                    517
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               517
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                517
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               517
         AG_TITLE = 'BOX'                                                 517
         AG_EXNAME = 'SHAPE'                                              517
         AG_PARLIST = 'DX_DY_DZ'                                          517
         CALL AXSHAPE                                                     517
C    *                                                                    517
C    Create BGMT :                                                        518
C    Call To Local Procedure BGMT                                         518
         ASSIGN 5480 TO BLOCK_BGMT                                        518
         GO TO  5470                                                      518
5480     CONTINUE                                                         518
C    *                                                                    518
C    Geant POSITION : BGMT ===> KONLY='MANY' <===                         519
         CALL AGSRESET                                                    519
         AG_KONLY = 'MANY'                                                519
         AG_TITLE = 'BGMT'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'KONLY'                                             519
         CALL AXPOSITION                                                  519
C    *                                                                    519
C    Create BRMD :                                                        520
C    Call To Local Procedure BRMD                                         520
         ASSIGN 5500 TO BLOCK_BRMD                                        520
         GO TO  5490                                                      520
5500     CONTINUE                                                         520
C    *                                                                    520
C    Check TOF==2                                                         521
         IF (TOF.EQ.2) THEN                                               521
         Z0 = TRAY_LENGTH/2 - 0.05                                        522
         X0 = -3.66                                                       523
C    *                                                                    524
         I=1                                                              524
            GO TO 5513                                                    524
5511        I=I+1                                                         524
5513        IF(I-(33).GT.0) GO TO 5512                                    524
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  525
C        ALPHAY=MODR_MRPCA(I) <===                                        525
            CALL AGSRESET                                                 525
            AG_X = X0+MODR_MRPCX(I)                                       525
            AG_Z = Z0-MODR_MRPCZ(I)                                       525
            AG_ALPHAN=AG_ALPHAN+1                                         525
            AG_ALPHAK(AG_ALPHAN)=2                                        525
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            525
            AG_TITLE = 'BRMD'                                             525
            AG_EXNAME = 'POSITION'                                        525
            AG_PARLIST = 'X_Z_ALPHAY'                                     525
            CALL AXPOSITION                                               525
C       *                                                                 525
         GO TO 5511                                                       526
5512     CONTINUE                                                         526
         ELSEIF (TOF.EQ.3) THEN                                           527
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       528
         X0 = -3.66                                                       529
C    *                                                                    530
         I=1                                                              530
            GO TO 5523                                                    530
5521        I=I+1                                                         530
5523        IF(I-(32).GT.0) GO TO 5522                                    530
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            531
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    531
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  532
C        ALPHAY=MOD4_MRPCA(I) <===                                        532
            CALL AGSRESET                                                 532
            AG_X = X0+MOD4_MRPCX(I)                                       532
            AG_Z = Z0-MOD4_MRPCZ(I)                                       532
            AG_ALPHAN=AG_ALPHAN+1                                         532
            AG_ALPHAK(AG_ALPHAN)=2                                        532
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            532
            AG_TITLE = 'BRMD'                                             532
            AG_EXNAME = 'POSITION'                                        532
            AG_PARLIST = 'X_Z_ALPHAY'                                     532
            CALL AXPOSITION                                               532
C       *                                                                 532
            ENDIF                                                         532
         GO TO 5521                                                       534
5522     CONTINUE                                                         534
         ELSEIF (TOF.EQ.4) THEN                                           535
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD5_TRAYEDGEZ                       536
C    LIUJ PUTS THIS NEW VALUE                                             537
         X0 = -3.90                                                       537
C    *                                                                    538
         I=1                                                              538
            GO TO 5533                                                    538
5531        I=I+1                                                         538
5533        IF(I-(32).GT.0) GO TO 5532                                    538
C       Geant POSITION : BRMD ===> X=X0+MOD5_MRPCX(I) Z=Z0-MOD5_MRPCZ(I)  539
C        ALPHAY=MOD5_MRPCA(I) <===                                        539
            CALL AGSRESET                                                 539
            AG_X = X0+MOD5_MRPCX(I)                                       539
            AG_Z = Z0-MOD5_MRPCZ(I)                                       539
            AG_ALPHAN=AG_ALPHAN+1                                         539
            AG_ALPHAK(AG_ALPHAN)=2                                        539
            AG_ALPHAL(AG_ALPHAN)=MOD5_MRPCA(I)                            539
            AG_TITLE = 'BRMD'                                             539
            AG_EXNAME = 'POSITION'                                        539
            AG_PARLIST = 'X_Z_ALPHAY'                                     539
            CALL AXPOSITION                                               539
C       *                                                                 539
         GO TO 5531                                                       540
5532     CONTINUE                                                         540
         ENDIF                                                            541
5211  CALL AGSPOPB                                                        542
      GO TO BLOCK_BRTC, (5210 ,5220)                                      542
C End of Local Procedure                                                  542
C ----------------------------------------------------------------------  543
C ------------ BGMT                                                       543
C Begin Local Procedure BGMT                                              544
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    544
5470  CONTINUE                                                            544
         AG_TITLE='BGMT'                                                  544
         AG_MODULE=MODULE                                                 544
         AG_MARK='V00'                                                    544
         CALL AGSPUSH                                                     544
         AG_MODULE=MODULE                                                 544
C    *                                                                    544
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       545
         CALL AGSRESET                                                    545
         AG_SEEN = 0                                                      545
         AG_COLO = 2                                                      545
         AG_TITLE = 'BGMT'                                                545
         AG_EXNAME = 'ATTRIBUTE'                                          545
         AG_PARLIST = 'SEEN_COLO'                                         545
         CALL AXATTRIBUTE                                                 545
C    *                                                                    545
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           545
         CALL AGSRESET                                                    546
         AG_TITLE = 'RPCGAS'                                              546
         AG_EXNAME = 'MATERIAL'                                           546
         AG_PARLIST = 'NONE'                                              546
         CALL AXMATERIAL                                                  546
C    *                                                                    546
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  546
C    /2-TRAY_WALLTHK-MODR_HBWID DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===        546
         CALL AGSRESET                                                    548
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               548
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-MODR_HBWID                     548
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               548
         AG_TITLE = 'BOX'                                                 548
         AG_EXNAME = 'SHAPE'                                              548
         AG_PARLIST = 'DX_DY_DZ'                                          548
         CALL AXSHAPE                                                     548
C    *                                                                    548
5471  CALL AGSPOPB                                                        549
      GO TO BLOCK_BGMT, (5470 ,5480)                                      549
C End of Local Procedure                                                  549
C ----------------------------------------------------------------------  550
C ------------ BMAA                                                       550
C Begin Local Procedure BMAA                                              551
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    551
5380  CONTINUE                                                            551
         AG_TITLE='BMAA'                                                  551
         AG_MODULE=MODULE                                                 551
         AG_MARK='V00'                                                    551
         CALL AGSPUSH                                                     551
         AG_MODULE=MODULE                                                 551
C    *                                                                    551
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       552
         CALL AGSRESET                                                    552
         AG_SEEN = 0                                                      552
         AG_COLO = 2                                                      552
         AG_TITLE = 'BMAA'                                                552
         AG_EXNAME = 'ATTRIBUTE'                                          552
         AG_PARLIST = 'SEEN_COLO'                                         552
         CALL AXATTRIBUTE                                                 552
C    *                                                                    552
C    Check IWID==4                                                        552
         IF (IWID.EQ.4) THEN                                              553
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  555
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          555
         CALL AGSRESET                                                    555
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               555
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      555
         AG_DZ = SUBLEN/2.                                                555
         AG_TITLE = 'BOX'                                                 555
         AG_EXNAME = 'SHAPE'                                              555
         AG_PARLIST = 'DX_DY_DZ'                                          555
         CALL AXSHAPE                                                     555
C    *                                                                    555
         ELSE                                                             555
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  558
C    DTH/2 DZ=15.5 <===                                                   558
         CALL AGSRESET                                                    558
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               558
         AG_DY = TOFF_BOXWIDTH/2                                          558
         AG_DZ = 15.5                                                     558
         AG_TITLE = 'BOX'                                                 558
         AG_EXNAME = 'SHAPE'                                              558
         AG_PARLIST = 'DX_DY_DZ'                                          558
         CALL AXSHAPE                                                     558
C    *                                                                    558
         ENDIF                                                            558
C    Create BMTD :                                                        560
C    Call To Local Procedure BMTD                                         560
         ASSIGN 5550 TO BLOCK_BMTD                                        560
         GO TO  5540                                                      560
5550     CONTINUE                                                         560
C    *                                                                    560
5381  CALL AGSPOPB                                                        561
      GO TO BLOCK_BMAA, (5380 ,5390,5400)                                 561
C End of Local Procedure                                                  561
C ----------------------------------------------------------------------  562
C ------------ BMTD                                                       562
C Begin Local Procedure BMTD                                              563
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         563
5540  CONTINUE                                                            563
         AG_TITLE='BMTD'                                                  563
         AG_MODULE=MODULE                                                 563
         AG_MARK='V00'                                                    563
         CALL AGSPUSH                                                     563
         AG_MODULE=MODULE                                                 563
C    *                                                                    563
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       564
         CALL AGSRESET                                                    564
         AG_SEEN = 1                                                      564
         AG_COLO = 1                                                      564
         AG_TITLE = 'BMTD'                                                564
         AG_EXNAME = 'ATTRIBUTE'                                          564
         AG_PARLIST = 'SEEN_COLO'                                         564
         CALL AXATTRIBUTE                                                 564
C    *                                                                    564
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   564
         CALL AGSRESET                                                    565
         AG_NDIV = IWID                                                   565
         AG_IAXIS = 2                                                     565
         AG_TITLE = 'DIVISION'                                            565
         AG_EXNAME = 'SHAPE'                                              565
         AG_PARLIST = 'NDIV_IAXIS'                                        565
         CALL AXSHAPE                                                     565
C    *                                                                    565
C    Create BASS :                                                        566
C    Call To Local Procedure BASS                                         566
         ASSIGN 5570 TO BLOCK_BASS                                        566
         GO TO  5560                                                      566
5570     CONTINUE                                                         566
C    *                                                                    566
C    Check IWID==5                                                        567
         IF (IWID.EQ.5) THEN                                              567
C    Geant POSITION : BASS ===> X=-1.7 ALPHAY=5.0 KONLY='MANY' <===       568
         CALL AGSRESET                                                    568
         AG_X = -1.7                                                      568
         AG_ALPHAN=AG_ALPHAN+1                                            568
         AG_ALPHAK(AG_ALPHAN)=2                                           568
         AG_ALPHAL(AG_ALPHAN)=5.0                                         568
         AG_KONLY = 'MANY'                                                568
         AG_TITLE = 'BASS'                                                568
         AG_EXNAME = 'POSITION'                                           568
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    568
         CALL AXPOSITION                                                  568
C    *                                                                    568
         ELSE                                                             568
C    Geant POSITION : BASS ===> X=-0.4 Z=TOFF_SLAT02Z-SUBCEN ALPHAY=10.0  570
C     KONLY='MANY' <===                                                   570
         CALL AGSRESET                                                    570
         AG_X = -0.4                                                      570
         AG_Z = TOFF_SLAT02Z-SUBCEN                                       570
         AG_ALPHAN=AG_ALPHAN+1                                            570
         AG_ALPHAK(AG_ALPHAN)=2                                           570
         AG_ALPHAL(AG_ALPHAN)=10.0                                        570
         AG_KONLY = 'MANY'                                                570
         AG_TITLE = 'BASS'                                                570
         AG_EXNAME = 'POSITION'                                           570
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  570
         CALL AXPOSITION                                                  570
C    *                                                                    570
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT03Z-SUBCEN ALPHAY=11.0  570
C     KONLY='MANY' <===                                                   570
         CALL AGSRESET                                                    571
         AG_X = -0.2                                                      571
         AG_Z = TOFF_SLAT03Z-SUBCEN                                       571
         AG_ALPHAN=AG_ALPHAN+1                                            571
         AG_ALPHAK(AG_ALPHAN)=2                                           571
         AG_ALPHAL(AG_ALPHAN)=11.0                                        571
         AG_KONLY = 'MANY'                                                571
         AG_TITLE = 'BASS'                                                571
         AG_EXNAME = 'POSITION'                                           571
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  571
         CALL AXPOSITION                                                  571
C    *                                                                    571
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT04Z-SUBCEN ALPHAY=11.0  571
C     KONLY='MANY' <===                                                   571
         CALL AGSRESET                                                    572
         AG_X = -0.2                                                      572
         AG_Z = TOFF_SLAT04Z-SUBCEN                                       572
         AG_ALPHAN=AG_ALPHAN+1                                            572
         AG_ALPHAK(AG_ALPHAN)=2                                           572
         AG_ALPHAL(AG_ALPHAN)=11.0                                        572
         AG_KONLY = 'MANY'                                                572
         AG_TITLE = 'BASS'                                                572
         AG_EXNAME = 'POSITION'                                           572
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  572
         CALL AXPOSITION                                                  572
C    *                                                                    572
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT05Z-SUBCEN ALPHAY=11.0  572
C     KONLY='MANY' <===                                                   572
         CALL AGSRESET                                                    573
         AG_X = -0.2                                                      573
         AG_Z = TOFF_SLAT05Z-SUBCEN                                       573
         AG_ALPHAN=AG_ALPHAN+1                                            573
         AG_ALPHAK(AG_ALPHAN)=2                                           573
         AG_ALPHAL(AG_ALPHAN)=11.0                                        573
         AG_KONLY = 'MANY'                                                573
         AG_TITLE = 'BASS'                                                573
         AG_EXNAME = 'POSITION'                                           573
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  573
         CALL AXPOSITION                                                  573
C    *                                                                    573
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT06Z-SUBCEN ALPHAY=11.0  573
C     KONLY='MANY' <===                                                   573
         CALL AGSRESET                                                    574
         AG_X = -0.2                                                      574
         AG_Z = TOFF_SLAT06Z-SUBCEN                                       574
         AG_ALPHAN=AG_ALPHAN+1                                            574
         AG_ALPHAK(AG_ALPHAN)=2                                           574
         AG_ALPHAL(AG_ALPHAN)=11.0                                        574
         AG_KONLY = 'MANY'                                                574
         AG_TITLE = 'BASS'                                                574
         AG_EXNAME = 'POSITION'                                           574
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  574
         CALL AXPOSITION                                                  574
C    *                                                                    574
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT07Z-SUBCEN ALPHAY=11.0  574
C     KONLY='MANY' <===                                                   574
         CALL AGSRESET                                                    575
         AG_X = -0.2                                                      575
         AG_Z = TOFF_SLAT07Z-SUBCEN                                       575
         AG_ALPHAN=AG_ALPHAN+1                                            575
         AG_ALPHAK(AG_ALPHAN)=2                                           575
         AG_ALPHAL(AG_ALPHAN)=11.0                                        575
         AG_KONLY = 'MANY'                                                575
         AG_TITLE = 'BASS'                                                575
         AG_EXNAME = 'POSITION'                                           575
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  575
         CALL AXPOSITION                                                  575
C    *                                                                    575
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT08Z-SUBCEN ALPHAY=11.0  575
C     KONLY='MANY' <===                                                   575
         CALL AGSRESET                                                    576
         AG_X = -0.2                                                      576
         AG_Z = TOFF_SLAT08Z-SUBCEN                                       576
         AG_ALPHAN=AG_ALPHAN+1                                            576
         AG_ALPHAK(AG_ALPHAN)=2                                           576
         AG_ALPHAL(AG_ALPHAN)=11.0                                        576
         AG_KONLY = 'MANY'                                                576
         AG_TITLE = 'BASS'                                                576
         AG_EXNAME = 'POSITION'                                           576
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  576
         CALL AXPOSITION                                                  576
C    *                                                                    576
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT09Z-SUBCEN ALPHAY=11.0  576
C     KONLY='MANY' <===                                                   576
         CALL AGSRESET                                                    577
         AG_X = -0.2                                                      577
         AG_Z = TOFF_SLAT09Z-SUBCEN                                       577
         AG_ALPHAN=AG_ALPHAN+1                                            577
         AG_ALPHAK(AG_ALPHAN)=2                                           577
         AG_ALPHAL(AG_ALPHAN)=11.0                                        577
         AG_KONLY = 'MANY'                                                577
         AG_TITLE = 'BASS'                                                577
         AG_EXNAME = 'POSITION'                                           577
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  577
         CALL AXPOSITION                                                  577
C    *                                                                    577
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT10Z-SUBCEN ALPHAY=11.0  577
C     KONLY='MANY' <===                                                   577
         CALL AGSRESET                                                    578
         AG_X = -0.2                                                      578
         AG_Z = TOFF_SLAT10Z-SUBCEN                                       578
         AG_ALPHAN=AG_ALPHAN+1                                            578
         AG_ALPHAK(AG_ALPHAN)=2                                           578
         AG_ALPHAL(AG_ALPHAN)=11.0                                        578
         AG_KONLY = 'MANY'                                                578
         AG_TITLE = 'BASS'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  578
         CALL AXPOSITION                                                  578
C    *                                                                    578
         ENDIF                                                            578
5541  CALL AGSPOPB                                                        580
      GO TO BLOCK_BMTD, (5540 ,5550)                                      580
C End of Local Procedure                                                  580
C ----------------------------------------------------------------------  581
C ------------ BASS                                                       581
C Begin Local Procedure BASS                                              582
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      582
5560  CONTINUE                                                            582
         AG_TITLE='BASS'                                                  582
         AG_MODULE=MODULE                                                 582
         AG_MARK='V00'                                                    582
         CALL AGSPUSH                                                     582
         AG_MODULE=MODULE                                                 582
C    *                                                                    582
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       583
         CALL AGSRESET                                                    583
         AG_SEEN = 0                                                      583
         AG_COLO = 6                                                      583
         AG_TITLE = 'BASS'                                                583
         AG_EXNAME = 'ATTRIBUTE'                                          583
         AG_PARLIST = 'SEEN_COLO'                                         583
         CALL AXATTRIBUTE                                                 583
C    *                                                                    583
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               583
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  586
C    K)/5., DZ=TOTLEN/2. <===                                             586
         CALL AGSRESET                                                    586
         AG_DX = TOFF_PMTMAXR                                             586
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           586
         AG_DZ = TOTLEN/2.                                                586
         AG_TITLE = 'BOX'                                                 586
         AG_EXNAME = 'SHAPE'                                              586
         AG_PARLIST = 'DX_DY_DZ'                                          586
         CALL AXSHAPE                                                     586
C    *                                                                    586
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               586
C    Create BCSB :                                                        588
C    Call To Local Procedure BCSB                                         588
         ASSIGN 5590 TO BLOCK_BCSB                                        588
         GO TO  5580                                                      588
5590     CONTINUE                                                         588
C    *                                                                    588
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               589
         CALL AGSRESET                                                    589
         AG_Z = ZPOS                                                      589
         AG_TITLE = 'BCSB'                                                589
         AG_EXNAME = 'POSITION'                                           589
         AG_PARLIST = 'Z'                                                 589
         CALL AXPOSITION                                                  589
C    *                                                                    589
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     589
C    Create BCPM :                                                        591
C    Call To Local Procedure BCPM                                         591
         ASSIGN 5600 TO BLOCK_BCPM                                        591
         GO TO  5310                                                      591
5600     CONTINUE                                                         591
C    *                                                                    591
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  592
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         592
         CALL AGSRESET                                                    592
         AG_X = 0                                                         592
         AG_Z = ZPOS                                                      592
         AG_RMIN = TOFF_PMTMINR                                           592
         AG_RMAX = TOFF_PMTMAXR                                           592
         AG_DZ = TOFF_PMTLEN/2                                            592
         AG_TITLE = 'BCPM'                                                592
         AG_EXNAME = 'POSITION'                                           592
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  592
         CALL AXPOSITION                                                  592
C    *                                                                    592
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     592
C    Create BTSK :                                                        594
C    Call To Local Procedure BTSK                                         594
         ASSIGN 5620 TO BLOCK_BTSK                                        594
         GO TO  5610                                                      594
5620     CONTINUE                                                         594
C    *                                                                    594
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  596
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       596
         CALL AGSRESET                                                    596
         AG_X = 0                                                         596
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     596
         AG_RMIN = TOFF_PMTMINR                                           596
         AG_RMAX = TOFF_PMTMAXR                                           596
         AG_DZ = TOFF_SOCKLEN/2                                           596
         AG_TITLE = 'BTSK'                                                596
         AG_EXNAME = 'POSITION'                                           596
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  596
         CALL AXPOSITION                                                  596
C    *                                                                    596
C    Create BCEL :                                                        597
C    Call To Local Procedure BCEL                                         597
         ASSIGN 5640 TO BLOCK_BCEL                                        597
         GO TO  5630                                                      597
5640     CONTINUE                                                         597
C    *                                                                    597
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  599
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                599
         CALL AGSRESET                                                    599
         AG_X = 0                                                         599
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    599
         AG_RMIN = 0                                                      599
         AG_RMAX = TOFF_PMTMINR                                           599
         AG_DZ = TOFF_ELECTHCK/2                                          599
         AG_TITLE = 'BCEL'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  599
         CALL AXPOSITION                                                  599
C    *                                                                    599
         ZPOS = ZPOS + TOFF_BASELEN/2                                     599
         AG_DZ = TOFF_BASELEN/2                                           601
         AG_DX = TOFF_ELECTHCK/2                                          601
         AG_DY = TOFF_CELLWID/2                                           601
C    Create BCEB :                                                        601
C    Call To Local Procedure BCEB                                         601
         ASSIGN 5660 TO BLOCK_BCEB                                        601
         GO TO  5650                                                      601
5660     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              602
         CALL AGSRESET                                                    602
         AG_X = TOFF_CELLHGT/2                                            602
         AG_Z = ZPOS                                                      602
         AG_TITLE = 'BCEB'                                                602
         AG_EXNAME = 'POSITION'                                           602
         AG_PARLIST = 'X_Z'                                               602
         CALL AXPOSITION                                                  602
C    *                                                                    602
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             602
         CALL AGSRESET                                                    603
         AG_X = -TOFF_CELLHGT/2                                           603
         AG_Z = ZPOS                                                      603
         AG_TITLE = 'BCEB'                                                603
         AG_EXNAME = 'POSITION'                                           603
         AG_PARLIST = 'X_Z'                                               603
         CALL AXPOSITION                                                  603
C    *                                                                    603
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               603
C    Create BCON :                                                        605
C    Call To Local Procedure BCON                                         605
         ASSIGN 5680 TO BLOCK_BCON                                        605
         GO TO  5670                                                      605
5680     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=0.6 DX=(TOFF_CELLHGT-T  606
C    OFF_ELECTHCK)/2 DY=1.25 <===                                         606
         CALL AGSRESET                                                    606
         AG_X = 0                                                         606
         AG_Y = 0                                                         606
         AG_Z = ZPOS                                                      606
         AG_DZ = 0.6                                                      606
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           606
         AG_DY = 1.25                                                     606
         AG_TITLE = 'BCON'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    606
         CALL AXPOSITION                                                  606
C    *                                                                    606
5561  CALL AGSPOPB                                                        607
      GO TO BLOCK_BASS, (5560 ,5570)                                      607
C End of Local Procedure                                                  607
C ----------------------------------------------------------------------  608
C ------------ BXSA                                                       608
C Begin Local Procedure BXSA                                              609
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    609
5270  CONTINUE                                                            609
         AG_TITLE='BXSA'                                                  609
         AG_MODULE=MODULE                                                 609
         AG_MARK='V00'                                                    609
         CALL AGSPUSH                                                     609
         AG_MODULE=MODULE                                                 609
C    *                                                                    609
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       610
         CALL AGSRESET                                                    610
         AG_SEEN = 0                                                      610
         AG_COLO = 3                                                      610
         AG_TITLE = 'BXSA'                                                610
         AG_EXNAME = 'ATTRIBUTE'                                          610
         AG_PARLIST = 'SEEN_COLO'                                         610
         CALL AXATTRIBUTE                                                 610
C    *                                                                    610
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       610
         CALL AGSRESET                                                    611
         AG_TITLE = 'POLYSTYREN'                                          611
         AG_EXNAME = 'MATERIAL'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXMATERIAL                                                  611
C    *                                                                    611
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           611
         CALL AGSRESET                                                    612
         AG_ISVOL = 1                                                     612
         AG_TITLE = 'SENSITIVE'                                           612
         AG_EXNAME = 'MEDIUM'                                             612
         AG_PARLIST = 'ISVOL'                                             612
         CALL AXMEDIUM                                                    612
C    *                                                                    612
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           612
         CALL AGSRESET                                                    613
         AG_DX = 0                                                        613
         AG_DY = 0                                                        613
         AG_DZ = 0                                                        613
         AG_TITLE = 'BOX'                                                 613
         AG_EXNAME = 'SHAPE'                                              613
         AG_PARLIST = 'DX_DY_DZ'                                          613
         CALL AXSHAPE                                                     613
C    *                                                                    613
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   613
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  613
C    SS:16:(0.0,0.01)                                                     613
         AG_TITLE = 'BXSA'                                                615
         AG_EXNAME = 'HITS'                                               615
         AG_TYPE=AG_MAGIC                                                 615
         CALL AGSDETV('BXSA')                                             615
C    *                                                                    615
         AG_PARLIST='X'                                                   615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = AG_MAGIC                                              615
         AG_BIN=0.01                                                      615
         AG_OPTION='S'                                                    615
         AG_FHMIN = AG_MAGIC                                              615
         AG_FHMAX = AG_MAGIC                                              615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='Y'                                                   615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = AG_MAGIC                                              615
         AG_BIN=0.01                                                      615
         AG_FHMIN=AG_MAGIC                                                615
         AG_FHMAX=AG_MAGIC                                                615
         AG_OPTION='*'                                                    615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='Z'                                                   615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = AG_MAGIC                                              615
         AG_BIN=0.01                                                      615
         AG_FHMIN=AG_MAGIC                                                615
         AG_FHMAX=AG_MAGIC                                                615
         AG_OPTION='*'                                                    615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='PTOT'                                                615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = 18                                                    615
         AG_BIN=AG_MAGIC                                                  615
         AG_OPTION='*'                                                    615
         AG_FHMIN = 0.0                                                   615
         AG_FHMAX = 100.0                                                 615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='CX'                                                  615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = 10                                                    615
         AG_BIN=AG_MAGIC                                                  615
         AG_FHMIN=AG_MAGIC                                                615
         AG_FHMAX=AG_MAGIC                                                615
         AG_OPTION='*'                                                    615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='CY'                                                  615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = 10                                                    615
         AG_BIN=AG_MAGIC                                                  615
         AG_FHMIN=AG_MAGIC                                                615
         AG_FHMAX=AG_MAGIC                                                615
         AG_OPTION='*'                                                    615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='CZ'                                                  615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = 10                                                    615
         AG_BIN=AG_MAGIC                                                  615
         AG_OPTION='*'                                                    615
         AG_FHMIN = AG_MAGIC                                              615
         AG_FHMAX = AG_MAGIC                                              615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='SLENG'                                               615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = AG_MAGIC                                              615
         AG_BIN=0.1                                                       615
         AG_OPTION='*'                                                    615
         AG_FHMIN = 0                                                     615
         AG_FHMAX = 500                                                   615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='TOF'                                                 615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = 16                                                    615
         AG_BIN=AG_MAGIC                                                  615
         AG_OPTION='*'                                                    615
         AG_FHMIN = 0.0                                                   615
         AG_FHMAX = 1.0E-6                                                615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='STEP'                                                615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = AG_MAGIC                                              615
         AG_BIN=0.01                                                      615
         AG_FHMIN=AG_MAGIC                                                615
         AG_FHMAX=AG_MAGIC                                                615
         AG_OPTION='*'                                                    615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         AG_PARLIST='ELOSS'                                               615
         AG_TITLE = 'DETH'                                                615
         AG_NBITS = 16                                                    615
         AG_BIN=AG_MAGIC                                                  615
         AG_OPTION='*'                                                    615
         AG_FHMIN = 0.0                                                   615
         AG_FHMAX = 0.01                                                  615
         CALL AGSDETH (0)                                                 615
C    *                                                                    615
         CALL AGSDETU('BXSA')                                             615
C    *                                                                    615
5271  CALL AGSPOPB                                                        616
      GO TO BLOCK_BXSA, (5270 ,5280,5370)                                 616
C End of Local Procedure                                                  616
C ----------------------------------------------------------------------  617
C ------------ BCSB                                                       617
C Begin Local Procedure BCSB                                              618
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    618
5580  CONTINUE                                                            618
         AG_TITLE='BCSB'                                                  618
         AG_MODULE=MODULE                                                 618
         AG_MARK='V00'                                                    618
         CALL AGSPUSH                                                     618
         AG_MODULE=MODULE                                                 618
C    *                                                                    618
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       619
         CALL AGSRESET                                                    619
         AG_SEEN = 1                                                      619
         AG_COLO = 7                                                      619
         AG_TITLE = 'BCSB'                                                619
         AG_EXNAME = 'ATTRIBUTE'                                          619
         AG_PARLIST = 'SEEN_COLO'                                         619
         CALL AXATTRIBUTE                                                 619
C    *                                                                    619
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       619
         CALL AGSRESET                                                    620
         AG_TITLE = 'POLYSTYREN'                                          620
         AG_EXNAME = 'MATERIAL'                                           620
         AG_PARLIST = 'NONE'                                              620
         CALL AXMATERIAL                                                  620
C    *                                                                    620
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           620
         CALL AGSRESET                                                    621
         AG_ISVOL = 1                                                     621
         AG_TITLE = 'SENSITIVE'                                           621
         AG_EXNAME = 'MEDIUM'                                             621
         AG_PARLIST = 'ISVOL'                                             621
         CALL AXMEDIUM                                                    621
C    *                                                                    621
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  621
C    F_SLATLEN/2 <===                                                     621
         CALL AGSRESET                                                    623
         AG_DX = TOFF_SLATTHCK/2                                          623
         AG_DY = TOFF_SLATWID/2                                           623
         AG_DZ = TOFF_SLATLEN/2                                           623
         AG_TITLE = 'BOX'                                                 623
         AG_EXNAME = 'SHAPE'                                              623
         AG_PARLIST = 'DX_DY_DZ'                                          623
         CALL AXSHAPE                                                     623
C    *                                                                    623
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   623
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  623
C    SS:16:(0.0,0.01)                                                     623
         AG_TITLE = 'BCSB'                                                625
         AG_EXNAME = 'HITS'                                               625
         AG_TYPE=AG_MAGIC                                                 625
         CALL AGSDETV('BCSB')                                             625
C    *                                                                    625
         AG_PARLIST='X'                                                   625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = AG_MAGIC                                              625
         AG_BIN=0.01                                                      625
         AG_OPTION='S'                                                    625
         AG_FHMIN = AG_MAGIC                                              625
         AG_FHMAX = AG_MAGIC                                              625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='Y'                                                   625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = AG_MAGIC                                              625
         AG_BIN=0.01                                                      625
         AG_FHMIN=AG_MAGIC                                                625
         AG_FHMAX=AG_MAGIC                                                625
         AG_OPTION='*'                                                    625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='Z'                                                   625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = AG_MAGIC                                              625
         AG_BIN=0.01                                                      625
         AG_FHMIN=AG_MAGIC                                                625
         AG_FHMAX=AG_MAGIC                                                625
         AG_OPTION='*'                                                    625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='PTOT'                                                625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = 18                                                    625
         AG_BIN=AG_MAGIC                                                  625
         AG_OPTION='*'                                                    625
         AG_FHMIN = 0.0                                                   625
         AG_FHMAX = 100.0                                                 625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='CX'                                                  625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = 10                                                    625
         AG_BIN=AG_MAGIC                                                  625
         AG_FHMIN=AG_MAGIC                                                625
         AG_FHMAX=AG_MAGIC                                                625
         AG_OPTION='*'                                                    625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='CY'                                                  625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = 10                                                    625
         AG_BIN=AG_MAGIC                                                  625
         AG_FHMIN=AG_MAGIC                                                625
         AG_FHMAX=AG_MAGIC                                                625
         AG_OPTION='*'                                                    625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='CZ'                                                  625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = 10                                                    625
         AG_BIN=AG_MAGIC                                                  625
         AG_OPTION='*'                                                    625
         AG_FHMIN = AG_MAGIC                                              625
         AG_FHMAX = AG_MAGIC                                              625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='SLENG'                                               625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = AG_MAGIC                                              625
         AG_BIN=0.1                                                       625
         AG_OPTION='*'                                                    625
         AG_FHMIN = 0                                                     625
         AG_FHMAX = 500                                                   625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='TOF'                                                 625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = 16                                                    625
         AG_BIN=AG_MAGIC                                                  625
         AG_OPTION='*'                                                    625
         AG_FHMIN = 0.0                                                   625
         AG_FHMAX = 1.0E-6                                                625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='STEP'                                                625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = AG_MAGIC                                              625
         AG_BIN=0.01                                                      625
         AG_FHMIN=AG_MAGIC                                                625
         AG_FHMAX=AG_MAGIC                                                625
         AG_OPTION='*'                                                    625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         AG_PARLIST='ELOSS'                                               625
         AG_TITLE = 'DETH'                                                625
         AG_NBITS = 16                                                    625
         AG_BIN=AG_MAGIC                                                  625
         AG_OPTION='*'                                                    625
         AG_FHMIN = 0.0                                                   625
         AG_FHMAX = 0.01                                                  625
         CALL AGSDETH (0)                                                 625
C    *                                                                    625
         CALL AGSDETU('BCSB')                                             625
C    *                                                                    625
5581  CALL AGSPOPB                                                        626
      GO TO BLOCK_BCSB, (5580 ,5590)                                      626
C End of Local Procedure                                                  626
C ----------------------------------------------------------------------  627
C ------------ BCCV                                                       627
C Begin Local Procedure BCCV                                              628
C BCCV IS A CTB OPTICAL CONVERTER                                         628
5290  CONTINUE                                                            628
         AG_TITLE='BCCV'                                                  628
         AG_MODULE=MODULE                                                 628
         AG_MARK='V00'                                                    628
         CALL AGSPUSH                                                     628
         AG_MODULE=MODULE                                                 628
C    *                                                                    628
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       629
         CALL AGSRESET                                                    629
         AG_SEEN = 0                                                      629
         AG_COLO = 3                                                      629
         AG_TITLE = 'BCCV'                                                629
         AG_EXNAME = 'ATTRIBUTE'                                          629
         AG_PARLIST = 'SEEN_COLO'                                         629
         CALL AXATTRIBUTE                                                 629
C    *                                                                    629
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       629
         CALL AGSRESET                                                    630
         AG_TITLE = 'POLYSTYREN'                                          630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            630
         CALL AGSRESET                                                    631
         AG_DX1 = 0                                                       631
         AG_DX2 = 0                                                       631
         AG_DY1 = 0                                                       631
         AG_DY2 = 0                                                       631
         AG_DZ = 0                                                        631
         AG_TITLE = 'TRD2'                                                631
         AG_EXNAME = 'SHAPE'                                              631
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                631
         CALL AXSHAPE                                                     631
C    *                                                                    631
5291  CALL AGSPOPB                                                        632
      GO TO BLOCK_BCCV, (5290 ,5300)                                      632
C End of Local Procedure                                                  632
C ----------------------------------------------------------------------  633
C ------------ BCSK                                                       633
C Begin Local Procedure BCSK                                              634
C BCSK IS A CTB LINEAR BASE TUBE                                          634
5330  CONTINUE                                                            634
         AG_TITLE='BCSK'                                                  634
         AG_MODULE=MODULE                                                 634
         AG_MARK='V00'                                                    634
         CALL AGSPUSH                                                     634
         AG_MODULE=MODULE                                                 634
C    *                                                                    634
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       635
         CALL AGSRESET                                                    635
         AG_SEEN = 0                                                      635
         AG_COLO = 2                                                      635
         AG_TITLE = 'BCSK'                                                635
         AG_EXNAME = 'ATTRIBUTE'                                          635
         AG_PARLIST = 'SEEN_COLO'                                         635
         CALL AXATTRIBUTE                                                 635
C    *                                                                    635
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       635
         CALL AGSRESET                                                    636
         AG_TITLE = 'POLYSTYREN'                                          636
         AG_EXNAME = 'MATERIAL'                                           636
         AG_PARLIST = 'NONE'                                              636
         CALL AXMATERIAL                                                  636
C    *                                                                    636
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      636
         CALL AGSRESET                                                    637
         AG_RMIN = 0                                                      637
         AG_RMAX = 0                                                      637
         AG_DZ = 0                                                        637
         AG_TITLE = 'TUBE'                                                637
         AG_EXNAME = 'SHAPE'                                              637
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      637
         CALL AXSHAPE                                                     637
C    *                                                                    637
5331  CALL AGSPOPB                                                        638
      GO TO BLOCK_BCSK, (5330 ,5340)                                      638
C End of Local Procedure                                                  638
C ----------------------------------------------------------------------  639
C ------------ BZEL                                                       639
C Begin Local Procedure BZEL                                              640
C BZEL IS A CTB PM ELECTRONICS                                            640
5350  CONTINUE                                                            640
         AG_TITLE='BZEL'                                                  640
         AG_MODULE=MODULE                                                 640
         AG_MARK='V00'                                                    640
         CALL AGSPUSH                                                     640
         AG_MODULE=MODULE                                                 640
C    *                                                                    640
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       641
         CALL AGSRESET                                                    641
         AG_SEEN = 0                                                      641
         AG_COLO = 6                                                      641
         AG_TITLE = 'BZEL'                                                641
         AG_EXNAME = 'ATTRIBUTE'                                          641
         AG_PARLIST = 'SEEN_COLO'                                         641
         CALL AXATTRIBUTE                                                 641
C    *                                                                    641
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          641
         CALL AGSRESET                                                    642
         AG_TITLE = 'SILICON'                                             642
         AG_EXNAME = 'MATERIAL'                                           642
         AG_PARLIST = 'NONE'                                              642
         CALL AXMATERIAL                                                  642
C    *                                                                    642
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           642
         CALL AGSRESET                                                    643
         AG_DX = 0                                                        643
         AG_DY = 0                                                        643
         AG_DZ = 0                                                        643
         AG_TITLE = 'BOX'                                                 643
         AG_EXNAME = 'SHAPE'                                              643
         AG_PARLIST = 'DX_DY_DZ'                                          643
         CALL AXSHAPE                                                     643
C    *                                                                    643
5351  CALL AGSPOPB                                                        644
      GO TO BLOCK_BZEL, (5350 ,5360)                                      644
C End of Local Procedure                                                  644
C ----------------------------------------------------------------------  645
C ------------ BCPM                                                       645
C Begin Local Procedure BCPM                                              646
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   646
5310  CONTINUE                                                            646
         AG_TITLE='BCPM'                                                  646
         AG_MODULE=MODULE                                                 646
         AG_MARK='V00'                                                    646
         CALL AGSPUSH                                                     646
         AG_MODULE=MODULE                                                 646
C    *                                                                    646
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       647
         CALL AGSRESET                                                    647
         AG_SEEN = 0                                                      647
         AG_COLO = 1                                                      647
         AG_TITLE = 'BCPM'                                                647
         AG_EXNAME = 'ATTRIBUTE'                                          647
         AG_PARLIST = 'SEEN_COLO'                                         647
         CALL AXATTRIBUTE                                                 647
C    *                                                                    647
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       647
         CALL AGSRESET                                                    648
         AG_TITLE = 'POLYSTYREN'                                          648
         AG_EXNAME = 'MATERIAL'                                           648
         AG_PARLIST = 'NONE'                                              648
         CALL AXMATERIAL                                                  648
C    *                                                                    648
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      648
         CALL AGSRESET                                                    649
         AG_RMIN = 0                                                      649
         AG_RMAX = 0                                                      649
         AG_DZ = 0                                                        649
         AG_TITLE = 'TUBE'                                                649
         AG_EXNAME = 'SHAPE'                                              649
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      649
         CALL AXSHAPE                                                     649
C    *                                                                    649
5311  CALL AGSPOPB                                                        650
      GO TO BLOCK_BCPM, (5310 ,5320,5600)                                 650
C End of Local Procedure                                                  650
C ----------------------------------------------------------------------  651
C ------------ BTSK                                                       651
C Begin Local Procedure BTSK                                              652
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                652
5610  CONTINUE                                                            652
         AG_TITLE='BTSK'                                                  652
         AG_MODULE=MODULE                                                 652
         AG_MARK='V00'                                                    652
         CALL AGSPUSH                                                     652
         AG_MODULE=MODULE                                                 652
C    *                                                                    652
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       653
         CALL AGSRESET                                                    653
         AG_SEEN = 0                                                      653
         AG_COLO = 7                                                      653
         AG_TITLE = 'BTSK'                                                653
         AG_EXNAME = 'ATTRIBUTE'                                          653
         AG_PARLIST = 'SEEN_COLO'                                         653
         CALL AXATTRIBUTE                                                 653
C    *                                                                    653
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       653
         CALL AGSRESET                                                    654
         AG_TITLE = 'POLYSTYREN'                                          654
         AG_EXNAME = 'MATERIAL'                                           654
         AG_PARLIST = 'NONE'                                              654
         CALL AXMATERIAL                                                  654
C    *                                                                    654
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      654
         CALL AGSRESET                                                    655
         AG_RMIN = 0                                                      655
         AG_RMAX = 0                                                      655
         AG_DZ = 0                                                        655
         AG_TITLE = 'TUBE'                                                655
         AG_EXNAME = 'SHAPE'                                              655
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      655
         CALL AXSHAPE                                                     655
C    *                                                                    655
5611  CALL AGSPOPB                                                        656
      GO TO BLOCK_BTSK, (5610 ,5620)                                      656
C End of Local Procedure                                                  656
C ----------------------------------------------------------------------  657
C ------------ BCEL                                                       657
C Begin Local Procedure BCEL                                              658
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     658
5630  CONTINUE                                                            658
         AG_TITLE='BCEL'                                                  658
         AG_MODULE=MODULE                                                 658
         AG_MARK='V00'                                                    658
         CALL AGSPUSH                                                     658
         AG_MODULE=MODULE                                                 658
C    *                                                                    658
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       659
         CALL AGSRESET                                                    659
         AG_SEEN = 0                                                      659
         AG_COLO = 3                                                      659
         AG_TITLE = 'BCEL'                                                659
         AG_EXNAME = 'ATTRIBUTE'                                          659
         AG_PARLIST = 'SEEN_COLO'                                         659
         CALL AXATTRIBUTE                                                 659
C    *                                                                    659
C    Geant MATERIAL : G10 ===> inherited arguments used <===              659
         CALL AGSRESET                                                    660
         AG_TITLE = 'G10'                                                 660
         AG_EXNAME = 'MATERIAL'                                           660
         AG_PARLIST = 'NONE'                                              660
         CALL AXMATERIAL                                                  660
C    *                                                                    660
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      660
         CALL AGSRESET                                                    661
         AG_RMIN = 0                                                      661
         AG_RMAX = 0                                                      661
         AG_DZ = 0                                                        661
         AG_TITLE = 'TUBE'                                                661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      661
         CALL AXSHAPE                                                     661
C    *                                                                    661
5631  CALL AGSPOPB                                                        662
      GO TO BLOCK_BCEL, (5630 ,5640)                                      662
C End of Local Procedure                                                  662
C ----------------------------------------------------------------------  663
C ------------ BCEB                                                       663
C Begin Local Procedure BCEB                                              664
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       664
5650  CONTINUE                                                            664
         AG_TITLE='BCEB'                                                  664
         AG_MODULE=MODULE                                                 664
         AG_MARK='V00'                                                    664
         CALL AGSPUSH                                                     664
         AG_MODULE=MODULE                                                 664
C    *                                                                    664
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       665
         CALL AGSRESET                                                    665
         AG_SEEN = 0                                                      665
         AG_COLO = 3                                                      665
         AG_TITLE = 'BCEL'                                                665
         AG_EXNAME = 'ATTRIBUTE'                                          665
         AG_PARLIST = 'SEEN_COLO'                                         665
         CALL AXATTRIBUTE                                                 665
C    *                                                                    665
C    Geant MATERIAL : G10 ===> inherited arguments used <===              665
         CALL AGSRESET                                                    666
         AG_TITLE = 'G10'                                                 666
         AG_EXNAME = 'MATERIAL'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXMATERIAL                                                  666
C    *                                                                    666
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           666
         CALL AGSRESET                                                    667
         AG_DX = 0                                                        667
         AG_DY = 0                                                        667
         AG_DZ = 0                                                        667
         AG_TITLE = 'BOX'                                                 667
         AG_EXNAME = 'SHAPE'                                              667
         AG_PARLIST = 'DX_DY_DZ'                                          667
         CALL AXSHAPE                                                     667
C    *                                                                    667
5651  CALL AGSPOPB                                                        668
      GO TO BLOCK_BCEB, (5650 ,5660)                                      668
C End of Local Procedure                                                  668
C ----------------------------------------------------------------------  669
C ------------ BPLA                                                       669
C Begin Local Procedure BPLA                                              670
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   670
5450  CONTINUE                                                            670
         AG_TITLE='BPLA'                                                  670
         AG_MODULE=MODULE                                                 670
         AG_MARK='V00'                                                    670
         CALL AGSPUSH                                                     670
         AG_MODULE=MODULE                                                 670
C    *                                                                    670
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       671
         CALL AGSRESET                                                    671
         AG_SEEN = 0                                                      671
         AG_COLO = 4                                                      671
         AG_TITLE = 'BPLA'                                                671
         AG_EXNAME = 'ATTRIBUTE'                                          671
         AG_PARLIST = 'SEEN_COLO'                                         671
         CALL AXATTRIBUTE                                                 671
C    *                                                                    671
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       671
         CALL AGSRESET                                                    672
         AG_TITLE = 'POLYSTYREN'                                          672
         AG_EXNAME = 'MATERIAL'                                           672
         AG_PARLIST = 'NONE'                                              672
         CALL AXMATERIAL                                                  672
C    *                                                                    672
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           672
         CALL AGSRESET                                                    673
         AG_DX = 0                                                        673
         AG_DY = 0                                                        673
         AG_DZ = 0                                                        673
         AG_TITLE = 'BOX'                                                 673
         AG_EXNAME = 'SHAPE'                                              673
         AG_PARLIST = 'DX_DY_DZ'                                          673
         CALL AXSHAPE                                                     673
C    *                                                                    673
C    Create BCON :                                                        674
C    Call To Local Procedure BCON                                         674
         ASSIGN 5690 TO BLOCK_BCON                                        674
         GO TO  5670                                                      674
5690     CONTINUE                                                         674
C    *                                                                    674
C    Geant POSITION : BCON ===> X=0 Y=0 Z=(-0.5*2.54)/2 DZ=0.5*2.54/2 DX  676
C    =0.08*2.54/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===                   676
         CALL AGSRESET                                                    676
         AG_X = 0                                                         676
         AG_Y = 0                                                         676
         AG_Z = (-0.5*2.54)/2                                             676
         AG_DZ = 0.5*2.54/2                                               676
         AG_DX = 0.08*2.54/2                                              676
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            676
         AG_TITLE = 'BCON'                                                676
         AG_EXNAME = 'POSITION'                                           676
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    676
         CALL AXPOSITION                                                  676
C    *                                                                    676
C    Geant POSITION : BCON ===> X=(-0.08*2.54-0.25*2.54)/2 Y=0 Z=(-0.08*  676
C    2.54)/2 DZ=0.08*2.54/2 DX=0.25*2.54/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-  676
C    2.0 <===                                                             676
         CALL AGSRESET                                                    678
         AG_X = (-0.08*2.54-0.25*2.54)/2                                  678
         AG_Y = 0                                                         678
         AG_Z = (-0.08*2.54)/2                                            678
         AG_DZ = 0.08*2.54/2                                              678
         AG_DX = 0.25*2.54/2                                              678
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            678
         AG_TITLE = 'BCON'                                                678
         AG_EXNAME = 'POSITION'                                           678
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    678
         CALL AXPOSITION                                                  678
C    *                                                                    678
5451  CALL AGSPOPB                                                        679
      GO TO BLOCK_BPLA, (5450 ,5460)                                      679
C End of Local Procedure                                                  679
C ----------------------------------------------------------------------  680
C ------------ BCON                                                       680
C Begin Local Procedure BCON                                              682
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  682
C NGLES, ETC......                                                        682
5670  CONTINUE                                                            682
         AG_TITLE='BCON'                                                  682
         AG_MODULE=MODULE                                                 682
         AG_MARK='V00'                                                    682
         CALL AGSPUSH                                                     682
         AG_MODULE=MODULE                                                 682
C    *                                                                    682
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       683
         CALL AGSRESET                                                    683
         AG_SEEN = 0                                                      683
         AG_COLO = 6                                                      683
         AG_TITLE = 'BCON'                                                683
         AG_EXNAME = 'ATTRIBUTE'                                          683
         AG_PARLIST = 'SEEN_COLO'                                         683
         CALL AXATTRIBUTE                                                 683
C    *                                                                    683
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       683
         CALL AGSRESET                                                    684
         AG_TITLE = 'POLYSTYREN'                                          684
         AG_EXNAME = 'MATERIAL'                                           684
         AG_PARLIST = 'NONE'                                              684
         CALL AXMATERIAL                                                  684
C    *                                                                    684
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           684
         CALL AGSRESET                                                    685
         AG_DX = 0                                                        685
         AG_DY = 0                                                        685
         AG_DZ = 0                                                        685
         AG_TITLE = 'BOX'                                                 685
         AG_EXNAME = 'SHAPE'                                              685
         AG_PARLIST = 'DX_DY_DZ'                                          685
         CALL AXSHAPE                                                     685
C    *                                                                    685
5671  CALL AGSPOPB                                                        686
      GO TO BLOCK_BCON, (5670 ,5680,5690)                                 686
C End of Local Procedure                                                  686
C ----------------------------------------------------------------------  687
C ------------ BFEE                                                       687
C Begin Local Procedure BFEE                                              688
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         688
5430  CONTINUE                                                            688
         AG_TITLE='BFEE'                                                  688
         AG_MODULE=MODULE                                                 688
         AG_MARK='V00'                                                    688
         CALL AGSPUSH                                                     688
         AG_MODULE=MODULE                                                 688
C    *                                                                    688
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       689
         CALL AGSRESET                                                    689
         AG_SEEN = 0                                                      689
         AG_COLO = 3                                                      689
         AG_TITLE = 'BFEE'                                                689
         AG_EXNAME = 'ATTRIBUTE'                                          689
         AG_PARLIST = 'SEEN_COLO'                                         689
         CALL AXATTRIBUTE                                                 689
C    *                                                                    689
C    Geant MATERIAL : G10 ===> inherited arguments used <===              689
         CALL AGSRESET                                                    690
         AG_TITLE = 'G10'                                                 690
         AG_EXNAME = 'MATERIAL'                                           690
         AG_PARLIST = 'NONE'                                              690
         CALL AXMATERIAL                                                  690
C    *                                                                    690
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           690
         CALL AGSRESET                                                    691
         AG_DX = 0                                                        691
         AG_DY = 0                                                        691
         AG_DZ = 0                                                        691
         AG_TITLE = 'BOX'                                                 691
         AG_EXNAME = 'SHAPE'                                              691
         AG_PARLIST = 'DX_DY_DZ'                                          691
         CALL AXSHAPE                                                     691
C    *                                                                    691
C    Create BLEM :                                                        692
C    Call To Local Procedure BLEM                                         692
         ASSIGN 5710 TO BLOCK_BLEM                                        692
         GO TO  5700                                                      692
5710     CONTINUE                                                         692
C    *                                                                    692
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-7.0 Z=2 <===   693
         CALL AGSRESET                                                    693
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     693
         AG_Y = -7.0                                                      693
         AG_Z = 2                                                         693
         AG_TITLE = 'BLEM'                                                693
         AG_EXNAME = 'POSITION'                                           693
         AG_PARLIST = 'X_Y_Z'                                             693
         CALL AXPOSITION                                                  693
C    *                                                                    693
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-3.5 Z=2 <===   693
         CALL AGSRESET                                                    694
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     694
         AG_Y = -3.5                                                      694
         AG_Z = 2                                                         694
         AG_TITLE = 'BLEM'                                                694
         AG_EXNAME = 'POSITION'                                           694
         AG_PARLIST = 'X_Y_Z'                                             694
         CALL AXPOSITION                                                  694
C    *                                                                    694
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=0. Z=2 <===     694
         CALL AGSRESET                                                    695
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     695
         AG_Y = 0.                                                        695
         AG_Z = 2                                                         695
         AG_TITLE = 'BLEM'                                                695
         AG_EXNAME = 'POSITION'                                           695
         AG_PARLIST = 'X_Y_Z'                                             695
         CALL AXPOSITION                                                  695
C    *                                                                    695
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=3.5 Z=2 <===    695
         CALL AGSRESET                                                    696
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     696
         AG_Y = 3.5                                                       696
         AG_Z = 2                                                         696
         AG_TITLE = 'BLEM'                                                696
         AG_EXNAME = 'POSITION'                                           696
         AG_PARLIST = 'X_Y_Z'                                             696
         CALL AXPOSITION                                                  696
C    *                                                                    696
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=7 Z=2 <===      696
         CALL AGSRESET                                                    697
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     697
         AG_Y = 7                                                         697
         AG_Z = 2                                                         697
         AG_TITLE = 'BLEM'                                                697
         AG_EXNAME = 'POSITION'                                           697
         AG_PARLIST = 'X_Y_Z'                                             697
         CALL AXPOSITION                                                  697
C    *                                                                    697
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-7.0 Z=-2 ALPH  697
C    AX=180 <===                                                          697
         CALL AGSRESET                                                    698
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     698
         AG_Y = -7.0                                                      698
         AG_Z = -2                                                        698
         AG_ALPHAN=AG_ALPHAN+1                                            698
         AG_ALPHAK(AG_ALPHAN)=1                                           698
         AG_ALPHAL(AG_ALPHAN)=180                                         698
         AG_TITLE = 'BLEM'                                                698
         AG_EXNAME = 'POSITION'                                           698
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      698
         CALL AXPOSITION                                                  698
C    *                                                                    698
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-3.5 Z=-2 ALPH  698
C    AX=180 <===                                                          698
         CALL AGSRESET                                                    699
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     699
         AG_Y = -3.5                                                      699
         AG_Z = -2                                                        699
         AG_ALPHAN=AG_ALPHAN+1                                            699
         AG_ALPHAK(AG_ALPHAN)=1                                           699
         AG_ALPHAL(AG_ALPHAN)=180                                         699
         AG_TITLE = 'BLEM'                                                699
         AG_EXNAME = 'POSITION'                                           699
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      699
         CALL AXPOSITION                                                  699
C    *                                                                    699
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=0. Z=-2 ALPHAX  699
C    =180 <===                                                            699
         CALL AGSRESET                                                    700
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     700
         AG_Y = 0.                                                        700
         AG_Z = -2                                                        700
         AG_ALPHAN=AG_ALPHAN+1                                            700
         AG_ALPHAK(AG_ALPHAN)=1                                           700
         AG_ALPHAL(AG_ALPHAN)=180                                         700
         AG_TITLE = 'BLEM'                                                700
         AG_EXNAME = 'POSITION'                                           700
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      700
         CALL AXPOSITION                                                  700
C    *                                                                    700
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=3.5 Z=-2 ALPHA  700
C    X=180 <===                                                           700
         CALL AGSRESET                                                    701
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     701
         AG_Y = 3.5                                                       701
         AG_Z = -2                                                        701
         AG_ALPHAN=AG_ALPHAN+1                                            701
         AG_ALPHAK(AG_ALPHAN)=1                                           701
         AG_ALPHAL(AG_ALPHAN)=180                                         701
         AG_TITLE = 'BLEM'                                                701
         AG_EXNAME = 'POSITION'                                           701
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      701
         CALL AXPOSITION                                                  701
C    *                                                                    701
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=7 Z=-2 ALPHAX=  701
C    180 <===                                                             701
         CALL AGSRESET                                                    702
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     702
         AG_Y = 7                                                         702
         AG_Z = -2                                                        702
         AG_ALPHAN=AG_ALPHAN+1                                            702
         AG_ALPHAK(AG_ALPHAN)=1                                           702
         AG_ALPHAL(AG_ALPHAN)=180                                         702
         AG_TITLE = 'BLEM'                                                702
         AG_EXNAME = 'POSITION'                                           702
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      702
         CALL AXPOSITION                                                  702
C    *                                                                    702
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-6.0 Z=-2 ALPH  702
C    AX=180 <===                                                          702
         CALL AGSRESET                                                    703
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     703
         AG_Y = -6.0                                                      703
         AG_Z = -2                                                        703
         AG_ALPHAN=AG_ALPHAN+1                                            703
         AG_ALPHAK(AG_ALPHAN)=1                                           703
         AG_ALPHAL(AG_ALPHAN)=180                                         703
         AG_TITLE = 'BLEM'                                                703
         AG_EXNAME = 'POSITION'                                           703
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      703
         CALL AXPOSITION                                                  703
C    *                                                                    703
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-2.5 Z=-2 ALPH  703
C    AX=180 <===                                                          703
         CALL AGSRESET                                                    704
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     704
         AG_Y = -2.5                                                      704
         AG_Z = -2                                                        704
         AG_ALPHAN=AG_ALPHAN+1                                            704
         AG_ALPHAK(AG_ALPHAN)=1                                           704
         AG_ALPHAL(AG_ALPHAN)=180                                         704
         AG_TITLE = 'BLEM'                                                704
         AG_EXNAME = 'POSITION'                                           704
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      704
         CALL AXPOSITION                                                  704
C    *                                                                    704
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=1. Z=-2 ALPHAX  704
C    =180 <===                                                            704
         CALL AGSRESET                                                    705
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     705
         AG_Y = 1.                                                        705
         AG_Z = -2                                                        705
         AG_ALPHAN=AG_ALPHAN+1                                            705
         AG_ALPHAK(AG_ALPHAN)=1                                           705
         AG_ALPHAL(AG_ALPHAN)=180                                         705
         AG_TITLE = 'BLEM'                                                705
         AG_EXNAME = 'POSITION'                                           705
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      705
         CALL AXPOSITION                                                  705
C    *                                                                    705
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=4.5 Z=-2 ALPHA  705
C    X=180 <===                                                           705
         CALL AGSRESET                                                    706
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     706
         AG_Y = 4.5                                                       706
         AG_Z = -2                                                        706
         AG_ALPHAN=AG_ALPHAN+1                                            706
         AG_ALPHAK(AG_ALPHAN)=1                                           706
         AG_ALPHAL(AG_ALPHAN)=180                                         706
         AG_TITLE = 'BLEM'                                                706
         AG_EXNAME = 'POSITION'                                           706
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      706
         CALL AXPOSITION                                                  706
C    *                                                                    706
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=8. Z=-2 ALPHAX  706
C    =180 <===                                                            706
         CALL AGSRESET                                                    707
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     707
         AG_Y = 8.                                                        707
         AG_Z = -2                                                        707
         AG_ALPHAN=AG_ALPHAN+1                                            707
         AG_ALPHAK(AG_ALPHAN)=1                                           707
         AG_ALPHAL(AG_ALPHAN)=180                                         707
         AG_TITLE = 'BLEM'                                                707
         AG_EXNAME = 'POSITION'                                           707
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      707
         CALL AXPOSITION                                                  707
C    *                                                                    707
5431  CALL AGSPOPB                                                        708
      GO TO BLOCK_BFEE, (5430 ,5440)                                      708
C End of Local Procedure                                                  708
C ----------------------------------------------------------------------  709
C ------------ BLEM                                                       709
C Begin Local Procedure BLEM                                              710
C BLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              710
5700  CONTINUE                                                            710
         AG_TITLE='BLEM'                                                  710
         AG_MODULE=MODULE                                                 710
         AG_MARK='V00'                                                    710
         CALL AGSPUSH                                                     710
         AG_MODULE=MODULE                                                 710
C    *                                                                    710
C    Geant ATTRIBUTE : BLEM ===> SEEN=0 COLO=3 <===                       711
         CALL AGSRESET                                                    711
         AG_SEEN = 0                                                      711
         AG_COLO = 3                                                      711
         AG_TITLE = 'BLEM'                                                711
         AG_EXNAME = 'ATTRIBUTE'                                          711
         AG_PARLIST = 'SEEN_COLO'                                         711
         CALL AXATTRIBUTE                                                 711
C    *                                                                    711
C    Geant SHAPE : BOX ===> DX=(0.68/2+(0.9-0.72)/2) DY=(0.68/2), DZ=(2.  711
C    0/2+(0.8+1.0)/2) <===                                                711
         CALL AGSRESET                                                    713
         AG_DX = (0.68/2+(0.9-0.72)/2)                                    713
         AG_DY = (0.68/2)                                                 713
         AG_DZ = (2.0/2+(0.8+1.0)/2)                                      713
         AG_TITLE = 'BOX'                                                 713
         AG_EXNAME = 'SHAPE'                                              713
         AG_PARLIST = 'DX_DY_DZ'                                          713
         CALL AXSHAPE                                                     713
C    *                                                                    713
C    Create BPIP :                                                        714
C    Call To Local Procedure BPIP                                         714
         ASSIGN 5730 TO BLOCK_BPIP                                        714
         GO TO  5720                                                      714
5730     CONTINUE                                                         714
C    *                                                                    714
C    Geant POSITION : BPIP ===> X=(0.9-0.72)/2 Y=0 Z=(0.8+1.0)/2 RMIN=0.  715
C    62/2 RMAX=0.68/2 DZ=2.0/2 <===                                       715
         CALL AGSRESET                                                    715
         AG_X = (0.9-0.72)/2                                              715
         AG_Y = 0                                                         715
         AG_Z = (0.8+1.0)/2                                               715
         AG_RMIN = 0.62/2                                                 715
         AG_RMAX = 0.68/2                                                 715
         AG_DZ = 2.0/2                                                    715
         AG_TITLE = 'BPIP'                                                715
         AG_EXNAME = 'POSITION'                                           715
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                715
         CALL AXPOSITION                                                  715
C    *                                                                    715
5701  CALL AGSPOPB                                                        716
      GO TO BLOCK_BLEM, (5700 ,5710)                                      716
C End of Local Procedure                                                  716
C ----------------------------------------------------------------------  717
C ------------ BCOO                                                       717
C Begin Local Procedure BCOO                                              718
C BCOO ARE THE COOLING RAILS/LOOPS                                        718
5410  CONTINUE                                                            718
         AG_TITLE='BCOO'                                                  718
         AG_MODULE=MODULE                                                 718
         AG_MARK='V00'                                                    718
         CALL AGSPUSH                                                     718
         AG_MODULE=MODULE                                                 718
C    *                                                                    718
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       719
         CALL AGSRESET                                                    719
         AG_SEEN = 0                                                      719
         AG_COLO = 2                                                      719
         AG_TITLE = 'BCOO'                                                719
         AG_EXNAME = 'ATTRIBUTE'                                          719
         AG_PARLIST = 'SEEN_COLO'                                         719
         CALL AXATTRIBUTE                                                 719
C    *                                                                    719
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           719
         CALL AGSRESET                                                    720
         AG_DX = 0                                                        720
         AG_DY = 0                                                        720
         AG_DZ = 0                                                        720
         AG_TITLE = 'BOX'                                                 720
         AG_EXNAME = 'SHAPE'                                              720
         AG_PARLIST = 'DX_DY_DZ'                                          720
         CALL AXSHAPE                                                     720
C    *                                                                    720
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               720
         AG_DX = TOFF_RAILTHCK/2                                          721
         AG_DY = TOFF_RAILWID/2                                           721
C    Create BRAI :                                                        721
C    Call To Local Procedure BRAI                                         721
         ASSIGN 5750 TO BLOCK_BRAI                                        721
         GO TO  5740                                                      721
5750     CONTINUE                                                         721
C    *                                                                    721
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  723
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  723
         CALL AGSRESET                                                    723
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                723
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                723
         AG_KONLY = 'MANY'                                                723
         AG_TITLE = 'BRAI'                                                723
         AG_EXNAME = 'POSITION'                                           723
         AG_PARLIST = 'X_Y_KONLY'                                         723
         CALL AXPOSITION                                                  723
C    *                                                                    723
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  723
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  723
C    ='MANY' <===                                                         723
         CALL AGSRESET                                                    725
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               725
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               725
         AG_ALPHAN=AG_ALPHAN+1                                            725
         AG_ALPHAK(AG_ALPHAN)=3                                           725
         AG_ALPHAL(AG_ALPHAN)=90                                          725
         AG_KONLY = 'MANY'                                                725
         AG_TITLE = 'BRAI'                                                725
         AG_EXNAME = 'POSITION'                                           725
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  725
         CALL AXPOSITION                                                  725
C    *                                                                    725
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  725
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 725
         CALL AGSRESET                                                    727
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                727
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               727
         AG_KONLY = 'MANY'                                                727
         AG_TITLE = 'BRAI'                                                727
         AG_EXNAME = 'POSITION'                                           727
         AG_PARLIST = 'X_Y_KONLY'                                         727
         CALL AXPOSITION                                                  727
C    *                                                                    727
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  727
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  727
C    Y='MANY' <===                                                        727
         CALL AGSRESET                                                    729
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               729
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              729
         AG_ALPHAN=AG_ALPHAN+1                                            729
         AG_ALPHAK(AG_ALPHAN)=3                                           729
         AG_ALPHAL(AG_ALPHAN)=90                                          729
         AG_KONLY = 'MANY'                                                729
         AG_TITLE = 'BRAI'                                                729
         AG_EXNAME = 'POSITION'                                           729
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  729
         CALL AXPOSITION                                                  729
C    *                                                                    729
         AG_RMAX = TOFF_COOLOUTR                                          729
         AG_RMIN = TOFF_COOLINNR                                          730
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               730
C    Create BPIP :                                                        730
C    Call To Local Procedure BPIP                                         730
         ASSIGN 5760 TO BLOCK_BPIP                                        730
         GO TO  5720                                                      730
5760     CONTINUE                                                         730
C    *                                                                    730
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  732
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   732
C    KONLY='MANY' <===                                                    732
         CALL AGSRESET                                                    732
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            732
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   732
         AG_KONLY = 'MANY'                                                732
         AG_TITLE = 'BPIP'                                                732
         AG_EXNAME = 'POSITION'                                           732
         AG_PARLIST = 'X_Y_KONLY'                                         732
         CALL AXPOSITION                                                  732
C    *                                                                    732
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  732
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  732
C     KONLY='MANY' <===                                                   732
         CALL AGSRESET                                                    734
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            734
         AG_Y = -(TRAY_WIDTH/                                             734
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      734
         AG_KONLY = 'MANY'                                                734
         AG_TITLE = 'BPIP'                                                734
         AG_EXNAME = 'POSITION'                                           734
         AG_PARLIST = 'X_Y_KONLY'                                         734
         CALL AXPOSITION                                                  734
C    *                                                                    734
         AG_RMAX = TOFF_COOLOUTR                                          734
         AG_RMIN = TOFF_COOLINNR                                          735
         AG_DZ = TRAY_WIDTH/                                              735
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    735
C    Create BPIQ :                                                        735
C    Call To Local Procedure BPIQ                                         735
         ASSIGN 5780 TO BLOCK_BPIQ                                        735
         GO TO  5770                                                      735
5780     CONTINUE                                                         735
C    *                                                                    735
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  738
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  738
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     738
         CALL AGSRESET                                                    738
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            738
         AG_Y = 0.0                                                       738
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    738
         AG_ALPHAN=AG_ALPHAN+1                                            738
         AG_ALPHAK(AG_ALPHAN)=1                                           738
         AG_ALPHAL(AG_ALPHAN)=90                                          738
         AG_KONLY = 'MANY'                                                738
         AG_TITLE = 'BPIQ'                                                738
         AG_EXNAME = 'POSITION'                                           738
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                738
         CALL AXPOSITION                                                  738
C    *                                                                    738
         AG_RMAX = TOFF_COOLINNR                                          738
         AG_RMIN = 0                                                      739
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               739
C    Create BWAT :                                                        739
C    Call To Local Procedure BWAT                                         739
         ASSIGN 5800 TO BLOCK_BWAT                                        739
         GO TO  5790                                                      739
5800     CONTINUE                                                         739
C    *                                                                    739
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  741
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   741
C    <===                                                                 741
         CALL AGSRESET                                                    741
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            741
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   741
         AG_TITLE = 'BWAT'                                                741
         AG_EXNAME = 'POSITION'                                           741
         AG_PARLIST = 'X_Y'                                               741
         CALL AXPOSITION                                                  741
C    *                                                                    741
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  741
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  741
C     <===                                                                741
         CALL AGSRESET                                                    743
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            743
         AG_Y = -(TRAY_WIDTH/                                             743
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      743
         AG_TITLE = 'BWAT'                                                743
         AG_EXNAME = 'POSITION'                                           743
         AG_PARLIST = 'X_Y'                                               743
         CALL AXPOSITION                                                  743
C    *                                                                    743
         AG_RMAX = TOFF_COOLINNR                                          743
         AG_RMIN = 0                                                      744
         AG_DZ = TRAY_WIDTH/                                              744
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    744
C    Create BWAT :                                                        744
C    Call To Local Procedure BWAT                                         744
         ASSIGN 5810 TO BLOCK_BWAT                                        744
         GO TO  5790                                                      744
5810     CONTINUE                                                         744
C    *                                                                    744
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  746
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  746
C    OUTR ALPHAX=90 <===                                                  746
         CALL AGSRESET                                                    746
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            746
         AG_Y = 0.0                                                       746
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    746
         AG_ALPHAN=AG_ALPHAN+1                                            746
         AG_ALPHAK(AG_ALPHAN)=1                                           746
         AG_ALPHAL(AG_ALPHAN)=90                                          746
         AG_TITLE = 'BWAT'                                                746
         AG_EXNAME = 'POSITION'                                           746
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      746
         CALL AXPOSITION                                                  746
C    *                                                                    746
5411  CALL AGSPOPB                                                        747
      GO TO BLOCK_BCOO, (5410 ,5420)                                      747
C End of Local Procedure                                                  747
C ----------------------------------------------------------------------  748
C ------------ BRAI                                                       748
C Begin Local Procedure BRAI                                              749
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   749
5740  CONTINUE                                                            749
         AG_TITLE='BRAI'                                                  749
         AG_MODULE=MODULE                                                 749
         AG_MARK='V00'                                                    749
         CALL AGSPUSH                                                     749
         AG_MODULE=MODULE                                                 749
C    *                                                                    749
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       750
         CALL AGSRESET                                                    750
         AG_SEEN = 0                                                      750
         AG_COLO = 7                                                      750
         AG_TITLE = 'BRAI'                                                750
         AG_EXNAME = 'ATTRIBUTE'                                          750
         AG_PARLIST = 'SEEN_COLO'                                         750
         CALL AXATTRIBUTE                                                 750
C    *                                                                    750
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        750
         CALL AGSRESET                                                    751
         AG_TITLE = 'ALUMINIUM'                                           751
         AG_EXNAME = 'MATERIAL'                                           751
         AG_PARLIST = 'NONE'                                              751
         CALL AXMATERIAL                                                  751
C    *                                                                    751
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     751
         CALL AGSRESET                                                    752
         AG_DX = 0.0                                                      752
         AG_DY = 0.0                                                      752
         AG_DZ = 0.0                                                      752
         AG_TITLE = 'BOX'                                                 752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'DX_DY_DZ'                                          752
         CALL AXSHAPE                                                     752
C    *                                                                    752
5741  CALL AGSPOPB                                                        753
      GO TO BLOCK_BRAI, (5740 ,5750)                                      753
C End of Local Procedure                                                  753
C ----------------------------------------------------------------------  754
C ------------ BPIP                                                       754
C Begin Local Procedure BPIP                                              755
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              755
5720  CONTINUE                                                            755
         AG_TITLE='BPIP'                                                  755
         AG_MODULE=MODULE                                                 755
         AG_MARK='V00'                                                    755
         CALL AGSPUSH                                                     755
         AG_MODULE=MODULE                                                 755
C    *                                                                    755
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       756
         CALL AGSRESET                                                    756
         AG_SEEN = 0                                                      756
         AG_COLO = 7                                                      756
         AG_TITLE = 'BPIP'                                                756
         AG_EXNAME = 'ATTRIBUTE'                                          756
         AG_PARLIST = 'SEEN_COLO'                                         756
         CALL AXATTRIBUTE                                                 756
C    *                                                                    756
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        756
         CALL AGSRESET                                                    757
         AG_TITLE = 'ALUMINIUM'                                           757
         AG_EXNAME = 'MATERIAL'                                           757
         AG_PARLIST = 'NONE'                                              757
         CALL AXMATERIAL                                                  757
C    *                                                                    757
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      757
         CALL AGSRESET                                                    758
         AG_RMIN = 0                                                      758
         AG_RMAX = 0                                                      758
         AG_DZ = 0                                                        758
         AG_TITLE = 'TUBE'                                                758
         AG_EXNAME = 'SHAPE'                                              758
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      758
         CALL AXSHAPE                                                     758
C    *                                                                    758
5721  CALL AGSPOPB                                                        759
      GO TO BLOCK_BPIP, (5720 ,5730,5760)                                 759
C End of Local Procedure                                                  759
C ----------------------------------------------------------------------  760
C ------------ BPIQ                                                       760
C Begin Local Procedure BPIQ                                              761
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             761
5770  CONTINUE                                                            761
         AG_TITLE='BPIQ'                                                  761
         AG_MODULE=MODULE                                                 761
         AG_MARK='V00'                                                    761
         CALL AGSPUSH                                                     761
         AG_MODULE=MODULE                                                 761
C    *                                                                    761
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       762
         CALL AGSRESET                                                    762
         AG_SEEN = 0                                                      762
         AG_COLO = 7                                                      762
         AG_TITLE = 'BPIQ'                                                762
         AG_EXNAME = 'ATTRIBUTE'                                          762
         AG_PARLIST = 'SEEN_COLO'                                         762
         CALL AXATTRIBUTE                                                 762
C    *                                                                    762
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        762
         CALL AGSRESET                                                    763
         AG_TITLE = 'ALUMINIUM'                                           763
         AG_EXNAME = 'MATERIAL'                                           763
         AG_PARLIST = 'NONE'                                              763
         CALL AXMATERIAL                                                  763
C    *                                                                    763
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      763
         CALL AGSRESET                                                    764
         AG_RMIN = 0                                                      764
         AG_RMAX = 0                                                      764
         AG_DZ = 0                                                        764
         AG_TITLE = 'TUBE'                                                764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      764
         CALL AXSHAPE                                                     764
C    *                                                                    764
5771  CALL AGSPOPB                                                        765
      GO TO BLOCK_BPIQ, (5770 ,5780)                                      765
C End of Local Procedure                                                  765
C ----------------------------------------------------------------------  766
C ------------ BUND                                                       766
C Begin Local Procedure BUND                                              767
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          767
5170  CONTINUE                                                            767
         AG_TITLE='BUND'                                                  767
         AG_MODULE=MODULE                                                 767
         AG_MARK='V00'                                                    767
         CALL AGSPUSH                                                     767
         AG_MODULE=MODULE                                                 767
C    *                                                                    767
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              768
         CALL AGSRESET                                                    768
         AG_SEEN = 0                                                      768
         AG_COLO = 1                                                      768
         AG_SERIAL = 0                                                    768
         AG_TITLE = 'BUND'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  768
C    SUPLEN/2 <===                                                        768
         CALL AGSRESET                                                    770
         AG_DX = TRAY_SUPFULLH/2                                          770
         AG_DY = TRAY_WIDTH/2                                             770
         AG_DZ = TRAY_SUPLEN/2                                            770
         AG_TITLE = 'BOX'                                                 770
         AG_EXNAME = 'SHAPE'                                              770
         AG_PARLIST = 'DX_DY_DZ'                                          770
         CALL AXSHAPE                                                     770
C    *                                                                    770
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        770
         CALL AGSRESET                                                    771
         AG_TITLE = 'ALUMINIUM'                                           771
         AG_EXNAME = 'MATERIAL'                                           771
         AG_PARLIST = 'NONE'                                              771
         CALL AXMATERIAL                                                  771
C    *                                                                    771
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           771
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             773
C    Create BTFT :                                                        774
C    Call To Local Procedure BTFT                                         774
         ASSIGN 5830 TO BLOCK_BTFT                                        774
         GO TO  5820                                                      774
5830     CONTINUE                                                         774
C    *                                                                    774
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  775
C    _FOOT1LEN/2 <===                                                     775
         CALL AGSRESET                                                    775
         AG_X = XPOS                                                      775
         AG_Y = -YPOS                                                     775
         AG_DX = TRAY_FOOTTHK/2                                           775
         AG_DY = TRAY_FOOT1LEN/2                                          775
         AG_TITLE = 'BTFT'                                                775
         AG_EXNAME = 'POSITION'                                           775
         AG_PARLIST = 'X_Y_DX_DY'                                         775
         CALL AXPOSITION                                                  775
C    *                                                                    775
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  775
C    _FOOT1LEN/2 <===                                                     775
         CALL AGSRESET                                                    776
         AG_X = XPOS                                                      776
         AG_Y = +YPOS                                                     776
         AG_DX = TRAY_FOOTTHK/2                                           776
         AG_DY = TRAY_FOOT1LEN/2                                          776
         AG_TITLE = 'BTFT'                                                776
         AG_EXNAME = 'POSITION'                                           776
         AG_PARLIST = 'X_Y_DX_DY'                                         776
         CALL AXPOSITION                                                  776
C    *                                                                    776
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          776
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   778
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  779
C    Y_FOOTTHK/2 <===                                                     779
         CALL AGSRESET                                                    779
         AG_X = XPOS                                                      779
         AG_Y = -YPOS                                                     779
         AG_DX = TRAY_FOOT2THK/2                                          779
         AG_DY = TRAY_FOOTTHK/2                                           779
         AG_TITLE = 'BTFT'                                                779
         AG_EXNAME = 'POSITION'                                           779
         AG_PARLIST = 'X_Y_DX_DY'                                         779
         CALL AXPOSITION                                                  779
C    *                                                                    779
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  779
C    Y_FOOTTHK/2 <===                                                     779
         CALL AGSRESET                                                    780
         AG_X = XPOS                                                      780
         AG_Y = +YPOS                                                     780
         AG_DX = TRAY_FOOT2THK/2                                          780
         AG_DY = TRAY_FOOTTHK/2                                           780
         AG_TITLE = 'BTFT'                                                780
         AG_EXNAME = 'POSITION'                                           780
         AG_PARLIST = 'X_Y_DX_DY'                                         780
         CALL AXPOSITION                                                  780
C    *                                                                    780
         XPOS =(TRAY_SUPFULLH + TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           780
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   782
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  783
C    _FOOT3LEN/2 <===                                                     783
         CALL AGSRESET                                                    783
         AG_X = XPOS                                                      783
         AG_Y = -YPOS                                                     783
         AG_DX = TRAY_FOOTTHK/2                                           783
         AG_DY = TRAY_FOOT3LEN/2                                          783
         AG_TITLE = 'BTFT'                                                783
         AG_EXNAME = 'POSITION'                                           783
         AG_PARLIST = 'X_Y_DX_DY'                                         783
         CALL AXPOSITION                                                  783
C    *                                                                    783
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  783
C    _FOOT3LEN/2 <===                                                     783
         CALL AGSRESET                                                    784
         AG_X = XPOS                                                      784
         AG_Y = +YPOS                                                     784
         AG_DX = TRAY_FOOTTHK/2                                           784
         AG_DY = TRAY_FOOT3LEN/2                                          784
         AG_TITLE = 'BTFT'                                                784
         AG_EXNAME = 'POSITION'                                           784
         AG_PARLIST = 'X_Y_DX_DY'                                         784
         CALL AXPOSITION                                                  784
C    *                                                                    784
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              784
     *   TAN(60*DEGRAD)                                                   784
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           786
     *   SUPPORT_AILE_WIDTH                                               786
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        787
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               788
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          789
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        790
         CALL AGSRESET                                                    790
         AG_TITLE = 'ALUMINIUM'                                           790
         AG_EXNAME = 'MATERIAL'                                           790
         AG_PARLIST = 'NONE'                                              790
         CALL AXMATERIAL                                                  790
C    *                                                                    790
C    Create BASE :                                                        791
C    Call To Local Procedure BASE                                         791
         ASSIGN 5850 TO BLOCK_BASE                                        791
         GO TO  5840                                                      791
5850     CONTINUE                                                         791
C    *                                                                    791
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   792
         CALL AGSRESET                                                    792
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          792
         AG_TITLE = 'BASE'                                                792
         AG_EXNAME = 'POSITION'                                           792
         AG_PARLIST = 'X'                                                 792
         CALL AXPOSITION                                                  792
C    *                                                                    792
C    Create BARM :                                                        793
C    Call To Local Procedure BARM                                         793
         ASSIGN 5870 TO BLOCK_BARM                                        793
         GO TO  5860                                                      793
5870     CONTINUE                                                         793
C    *                                                                    793
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  794
C    ===                                                                  794
         CALL AGSRESET                                                    794
         AG_X = SUPPORT_ARM_XPOS                                          794
         AG_Y = -SUPPORT_ARM_YPOS                                         794
         AG_TITLE = 'BARM'                                                794
         AG_EXNAME = 'POSITION'                                           794
         AG_PARLIST = 'X_Y'                                               794
         CALL AXPOSITION                                                  794
C    *                                                                    794
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  794
C    ===                                                                  794
         CALL AGSRESET                                                    795
         AG_X = SUPPORT_ARM_XPOS                                          795
         AG_Y = +SUPPORT_ARM_YPOS                                         795
         AG_TITLE = 'BARM'                                                795
         AG_EXNAME = 'POSITION'                                           795
         AG_PARLIST = 'X_Y'                                               795
         CALL AXPOSITION                                                  795
C    *                                                                    795
C    Create BANG :                                                        796
C    Call To Local Procedure BANG                                         796
         ASSIGN 5890 TO BLOCK_BANG                                        796
         GO TO  5880                                                      796
5890     CONTINUE                                                         796
C    *                                                                    796
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 797
         CALL AGSRESET                                                    797
         AG_Y = -SUPPORT_AILE_YPOS                                        797
         AG_TITLE = 'BANG'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'Y'                                                 797
         CALL AXPOSITION                                                  797
C    *                                                                    797
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      797
         CALL AGSRESET                                                    798
         AG_Y = +SUPPORT_AILE_YPOS                                        798
         AG_THETAX = 270                                                  798
         AG_TITLE = 'BANG'                                                798
         AG_EXNAME = 'POSITION'                                           798
         AG_PARLIST = 'Y_THETAX'                                          798
         CALL AXPOSITION                                                  798
C    *                                                                    798
C    Create BCOV :                                                        799
C    Call To Local Procedure BCOV                                         799
         ASSIGN 5910 TO BLOCK_BCOV                                        799
         GO TO  5900                                                      799
5910     CONTINUE                                                         799
C    *                                                                    799
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     800
         CALL AGSRESET                                                    800
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            800
         AG_TITLE = 'BCOV'                                                800
         AG_EXNAME = 'POSITION'                                           800
         AG_PARLIST = 'X'                                                 800
         CALL AXPOSITION                                                  800
C    *                                                                    800
5171  CALL AGSPOPB                                                        801
      GO TO BLOCK_BUND, (5170 ,5180)                                      801
C End of Local Procedure                                                  801
C ----------------------------------------------------------------------  802
C ------------ BTFT                                                       802
C Begin Local Procedure BTFT                                              803
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       803
5820  CONTINUE                                                            803
         AG_TITLE='BTFT'                                                  803
         AG_MODULE=MODULE                                                 803
         AG_MARK='V00'                                                    803
         CALL AGSPUSH                                                     803
         AG_MODULE=MODULE                                                 803
C    *                                                                    803
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       804
         CALL AGSRESET                                                    804
         AG_SEEN = 0                                                      804
         AG_COLO = 2                                                      804
         AG_TITLE = 'BTFT'                                                804
         AG_EXNAME = 'ATTRIBUTE'                                          804
         AG_PARLIST = 'SEEN_COLO'                                         804
         CALL AXATTRIBUTE                                                 804
C    *                                                                    804
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     804
         CALL AGSRESET                                                    805
         AG_DX = 0.0                                                      805
         AG_DY = 0.0                                                      805
         AG_DZ = 0.0                                                      805
         AG_TITLE = 'BOX'                                                 805
         AG_EXNAME = 'SHAPE'                                              805
         AG_PARLIST = 'DX_DY_DZ'                                          805
         CALL AXSHAPE                                                     805
C    *                                                                    805
5821  CALL AGSPOPB                                                        806
      GO TO BLOCK_BTFT, (5820 ,5830)                                      806
C End of Local Procedure                                                  806
C ----------------------------------------------------------------------  807
C ------------ BARM                                                       807
C Begin Local Procedure BARM                                              808
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              808
5860  CONTINUE                                                            808
         AG_TITLE='BARM'                                                  808
         AG_MODULE=MODULE                                                 808
         AG_MARK='V00'                                                    808
         CALL AGSPUSH                                                     808
         AG_MODULE=MODULE                                                 808
C    *                                                                    808
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       809
         CALL AGSRESET                                                    809
         AG_SEEN = 0                                                      809
         AG_COLO = 2                                                      809
         AG_TITLE = 'BARM'                                                809
         AG_EXNAME = 'ATTRIBUTE'                                          809
         AG_PARLIST = 'SEEN_COLO'                                         809
         CALL AXATTRIBUTE                                                 809
C    *                                                                    809
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  809
C    =                                                                    809
         CALL AGSRESET                                                    810
         AG_DX = TRAY_SUPARMT/2                                           810
         AG_DY = SUPPORT_ARM_WIDTH/2                                      810
         AG_TITLE = 'BOX'                                                 810
         AG_EXNAME = 'SHAPE'                                              810
         AG_PARLIST = 'DX_DY'                                             810
         CALL AXSHAPE                                                     810
C    *                                                                    810
5861  CALL AGSPOPB                                                        811
      GO TO BLOCK_BARM, (5860 ,5870)                                      811
C End of Local Procedure                                                  811
C ----------------------------------------------------------------------  812
C ------------ BANG                                                       812
C Begin Local Procedure BANG                                              813
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                813
5880  CONTINUE                                                            813
         AG_TITLE='BANG'                                                  813
         AG_MODULE=MODULE                                                 813
         AG_MARK='V00'                                                    813
         CALL AGSPUSH                                                     813
         AG_MODULE=MODULE                                                 813
C    *                                                                    813
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       814
         CALL AGSRESET                                                    814
         AG_SEEN = 0                                                      814
         AG_COLO = 2                                                      814
         AG_TITLE = 'BANG'                                                814
         AG_EXNAME = 'ATTRIBUTE'                                          814
         AG_PARLIST = 'SEEN_COLO'                                         814
         CALL AXATTRIBUTE                                                 814
C    *                                                                    814
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   814
C    ALPH=-60 THET=0 PHI=0 <===                                           814
         CALL AGSRESET                                                    816
         AG_DX = TRAY_SUPARMT/2                                           816
         AG_DY = SUPPORT_AILE_WIDTH/2                                     816
         AG_ALPH = -60                                                    816
         AG_THET = 0                                                      816
         AG_PHI = 0                                                       816
         AG_TITLE = 'PARA'                                                816
         AG_EXNAME = 'SHAPE'                                              816
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               816
         CALL AXSHAPE                                                     816
C    *                                                                    816
5881  CALL AGSPOPB                                                        817
      GO TO BLOCK_BANG, (5880 ,5890)                                      817
C End of Local Procedure                                                  817
C ----------------------------------------------------------------------  818
C ------------ BASE                                                       818
C Begin Local Procedure BASE                                              819
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               819
5840  CONTINUE                                                            819
         AG_TITLE='BASE'                                                  819
         AG_MODULE=MODULE                                                 819
         AG_MARK='V00'                                                    819
         CALL AGSPUSH                                                     819
         AG_MODULE=MODULE                                                 819
C    *                                                                    819
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       820
         CALL AGSRESET                                                    820
         AG_SEEN = 0                                                      820
         AG_COLO = 2                                                      820
         AG_TITLE = 'BASE'                                                820
         AG_EXNAME = 'ATTRIBUTE'                                          820
         AG_PARLIST = 'SEEN_COLO'                                         820
         CALL AXATTRIBUTE                                                 820
C    *                                                                    820
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    820
         CALL AGSRESET                                                    821
         AG_DX = TRAY_SUPBASET/2                                          821
         AG_DY = TRAY_SUPBASEW/2                                          821
         AG_TITLE = 'BOX'                                                 821
         AG_EXNAME = 'SHAPE'                                              821
         AG_PARLIST = 'DX_DY'                                             821
         CALL AXSHAPE                                                     821
C    *                                                                    821
5841  CALL AGSPOPB                                                        822
      GO TO BLOCK_BASE, (5840 ,5850)                                      822
C End of Local Procedure                                                  822
C ----------------------------------------------------------------------  823
C ------------ BCOV                                                       823
C Begin Local Procedure BCOV                                              824
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     824
5900  CONTINUE                                                            824
         AG_TITLE='BCOV'                                                  824
         AG_MODULE=MODULE                                                 824
         AG_MARK='V00'                                                    824
         CALL AGSPUSH                                                     824
         AG_MODULE=MODULE                                                 824
C    *                                                                    824
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       825
         CALL AGSRESET                                                    825
         AG_SEEN = 0                                                      825
         AG_COLO = 2                                                      825
         AG_TITLE = 'BCOV'                                                825
         AG_EXNAME = 'ATTRIBUTE'                                          825
         AG_PARLIST = 'SEEN_COLO'                                         825
         CALL AXATTRIBUTE                                                 825
C    *                                                                    825
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               825
         CALL AGSRESET                                                    826
         AG_RMIN = 0                                                      826
         AG_RMAX = TRAY_COOLOUTR                                          826
         AG_TITLE = 'TUBE'                                                826
         AG_EXNAME = 'SHAPE'                                              826
         AG_PARLIST = 'RMIN_RMAX'                                         826
         CALL AXSHAPE                                                     826
C    *                                                                    826
         AG_RMIN = 0                                                      826
         AG_RMAX = TRAY_COOLINNR                                          827
C    Create BWAT :                                                        827
C    Call To Local Procedure BWAT                                         827
         ASSIGN 5920 TO BLOCK_BWAT                                        827
         GO TO  5790                                                      827
5920     CONTINUE                                                         827
C    *                                                                    827
C    Geant POSITION : BWAT ===> inherited arguments used <===             828
         CALL AGSRESET                                                    828
         AG_TITLE = 'BWAT'                                                828
         AG_EXNAME = 'POSITION'                                           828
         AG_PARLIST = 'NONE'                                              828
         CALL AXPOSITION                                                  828
C    *                                                                    828
5901  CALL AGSPOPB                                                        829
      GO TO BLOCK_BCOV, (5900 ,5910)                                      829
C End of Local Procedure                                                  829
C ----------------------------------------------------------------------  830
C ------------ BWAT                                                       830
C Begin Local Procedure BWAT                                              831
C BWAT IS TPC COOLING WATER                                               831
5790  CONTINUE                                                            831
         AG_TITLE='BWAT'                                                  831
         AG_MODULE=MODULE                                                 831
         AG_MARK='V00'                                                    831
         CALL AGSPUSH                                                     831
         AG_MODULE=MODULE                                                 831
C    *                                                                    831
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       832
         CALL AGSRESET                                                    832
         AG_SEEN = 0                                                      832
         AG_COLO = 3                                                      832
         AG_TITLE = 'BWAT'                                                832
         AG_EXNAME = 'ATTRIBUTE'                                          832
         AG_PARLIST = 'SEEN_COLO'                                         832
         CALL AXATTRIBUTE                                                 832
C    *                                                                    832
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           832
         CALL AGSRESET                                                    833
         AG_A = 1                                                         833
         AG_Z = 1                                                         833
         AG_W = 2                                                         833
         AG_TITLE = 'H2'                                                  833
         AG_EXNAME = 'COMPONENT'                                          833
         AG_PARLIST = 'A_Z_W'                                             833
         CALL AXCOMPONENT                                                 833
C    *                                                                    833
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           833
         CALL AGSRESET                                                    834
         AG_A = 16                                                        834
         AG_Z = 8                                                         834
         AG_W = 1                                                         834
         AG_TITLE = 'O'                                                   834
         AG_EXNAME = 'COMPONENT'                                          834
         AG_PARLIST = 'A_Z_W'                                             834
         CALL AXCOMPONENT                                                 834
C    *                                                                    834
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             834
         CALL AGSRESET                                                    835
         AG_DENS = 1.0                                                    835
         AG_TITLE = 'WATER'                                               835
         AG_EXNAME = 'MIXTURE'                                            835
         AG_PARLIST = 'DENS'                                              835
         CALL AXMIXTURE                                                   835
C    *                                                                    835
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 <===                           835
         CALL AGSRESET                                                    836
         AG_RMIN = 0                                                      836
         AG_RMAX = 0                                                      836
         AG_TITLE = 'TUBE'                                                836
         AG_EXNAME = 'SHAPE'                                              836
         AG_PARLIST = 'RMIN_RMAX'                                         836
         CALL AXSHAPE                                                     836
C    *                                                                    836
5791  CALL AGSPOPB                                                        837
      GO TO BLOCK_BWAT, (5790 ,5800,5810,5920)                            837
C End of Local Procedure                                                  837
C ----------------------------------------------------------------------  838
C ------------ BRMD                                                       838
C Begin Local Procedure BRMD                                              839
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   839
5490  CONTINUE                                                            839
         AG_TITLE='BRMD'                                                  839
         AG_MODULE=MODULE                                                 839
         AG_MARK='V00'                                                    839
         CALL AGSPUSH                                                     839
         AG_MODULE=MODULE                                                 839
C    *                                                                    839
C    Geant ATTRIBUTE : BRMD ===> SEEN=0 COLO=6 <===                       840
         CALL AGSRESET                                                    840
         AG_SEEN = 0                                                      840
         AG_COLO = 6                                                      840
         AG_TITLE = 'BRMD'                                                840
         AG_EXNAME = 'ATTRIBUTE'                                          840
         AG_PARLIST = 'SEEN_COLO'                                         840
         CALL AXATTRIBUTE                                                 840
C    *                                                                    840
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           840
         CALL AGSRESET                                                    841
         AG_TITLE = 'RPCGAS'                                              841
         AG_EXNAME = 'MATERIAL'                                           841
         AG_PARLIST = 'NONE'                                              841
         CALL AXMATERIAL                                                  841
C    *                                                                    841
C    Geant SHAPE : BOX ===> DX=MODR_HEIGHT/2 DY=MODR_WIDTH/2, DZ=MODR_LE  841
C    NGTH/2 <===                                                          841
         CALL AGSRESET                                                    843
         AG_DX = MODR_HEIGHT/2                                            843
         AG_DY = MODR_WIDTH/2                                             843
         AG_DZ = MODR_LENGTH/2                                            843
         AG_TITLE = 'BOX'                                                 843
         AG_EXNAME = 'SHAPE'                                              843
         AG_PARLIST = 'DX_DY_DZ'                                          843
         CALL AXSHAPE                                                     843
C    *                                                                    843
C    Create BRHC :                                                        844
C    Call To Local Procedure BRHC                                         844
         ASSIGN 5940 TO BLOCK_BRHC                                        844
         GO TO  5930                                                      844
5940     CONTINUE                                                         844
C    *                                                                    844
C    Create BRCB :                                                        845
C    Call To Local Procedure BRCB                                         845
         ASSIGN 5960 TO BLOCK_BRCB                                        845
         GO TO  5950                                                      845
5960     CONTINUE                                                         845
C    *                                                                    845
C    Create BRMY :                                                        846
C    Call To Local Procedure BRMY                                         846
         ASSIGN 5980 TO BLOCK_BRMY                                        846
         GO TO  5970                                                      846
5980     CONTINUE                                                         846
C    *                                                                    846
C    Create BRGR :                                                        847
C    Call To Local Procedure BRGR                                         847
         ASSIGN 6000 TO BLOCK_BRGR                                        847
         GO TO  5990                                                      847
6000     CONTINUE                                                         847
C    *                                                                    847
C    Create BROG :                                                        848
C    Call To Local Procedure BROG                                         848
         ASSIGN 6020 TO BLOCK_BROG                                        848
         GO TO  6010                                                      848
6020     CONTINUE                                                         848
C    *                                                                    848
         DTHGT =(MODR_IGHGT + MODR_SPRMAX*2)*(MODR_NGAP - 1) +            849
     *   MODR_SPRMAX*2                                                    849
         AG_DZ = MODR_IGLEN/2.                                            850
         AG_KONLY = 'MANY'                                                850
         AG_DX = DTHGT/2.                                                 850
         AG_DY = MODR_IGWID/2.                                            850
C    Create BRDT :                                                        850
C    Call To Local Procedure BRDT                                         850
         ASSIGN 6040 TO BLOCK_BRDT                                        850
         GO TO  6030                                                      850
6040     CONTINUE                                                         850
C    *                                                                    850
C    Create BRWG :                                                        851
C    Call To Local Procedure BRWG                                         851
         ASSIGN 6060 TO BLOCK_BRWG                                        851
         GO TO  6050                                                      851
6060     CONTINUE                                                         851
C    *                                                                    851
         XPOS = MODR_HEIGHT/2.                                            852
         Y = 0.                                                           853
         Z = MODR_CENTER                                                  854
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    855
         CALL AGSRESET                                                    855
         AG_X = XPOS-MODR_HCHGT/2                                         855
         AG_Z = MODR_CENTER                                               855
         AG_TITLE = 'BRHC'                                                855
         AG_EXNAME = 'POSITION'                                           855
         AG_PARLIST = 'X_Z'                                               855
         CALL AXPOSITION                                                  855
C    *                                                                    855
         XPOS = XPOS - MODR_HCHGT                                         855
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            857
         CALL AGSRESET                                                    857
         AG_X = XPOS-MODR_PCBHGT/2                                        857
         AG_Z = 0.                                                        857
         AG_TITLE = 'BRCB'                                                857
         AG_EXNAME = 'POSITION'                                           857
         AG_PARLIST = 'X_Z'                                               857
         CALL AXPOSITION                                                  857
C    *                                                                    857
         XPOS = XPOS - MODR_PCBHGT                                        857
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 Z=MODR_CENTER <===    859
         CALL AGSRESET                                                    859
         AG_X = XPOS-MODR_MYHGT/2                                         859
         AG_Z = MODR_CENTER                                               859
         AG_TITLE = 'BRMY'                                                859
         AG_EXNAME = 'POSITION'                                           859
         AG_PARLIST = 'X_Z'                                               859
         CALL AXPOSITION                                                  859
C    *                                                                    859
         XPOS = XPOS - MODR_MYHGT                                         859
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  861
         CALL AGSRESET                                                    861
         AG_X = XPOS-MODR_GRHGT/2                                         861
         AG_TITLE = 'BRGR'                                                861
         AG_EXNAME = 'POSITION'                                           861
         AG_PARLIST = 'X'                                                 861
         CALL AXPOSITION                                                  861
C    *                                                                    861
         XPOS = XPOS - MODR_GRHGT                                         861
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  863
         CALL AGSRESET                                                    863
         AG_X = XPOS-MODR_OGHGT/2                                         863
         AG_TITLE = 'BROG'                                                863
         AG_EXNAME = 'POSITION'                                           863
         AG_PARLIST = 'X'                                                 863
         CALL AXPOSITION                                                  863
C    *                                                                    863
         XPOS = XPOS - MODR_OGHGT                                         863
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 <===                       865
         CALL AGSRESET                                                    865
         AG_X = XPOS-DTHGT/2                                              865
         AG_TITLE = 'BRDT'                                                865
         AG_EXNAME = 'POSITION'                                           865
         AG_PARLIST = 'X'                                                 865
         CALL AXPOSITION                                                  865
C    *                                                                    865
         XPOS = XPOS - DTHGT                                              865
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  867
         CALL AGSRESET                                                    867
         AG_X = XPOS-MODR_OGHGT/2                                         867
         AG_TITLE = 'BROG'                                                867
         AG_EXNAME = 'POSITION'                                           867
         AG_PARLIST = 'X'                                                 867
         CALL AXPOSITION                                                  867
C    *                                                                    867
         XPOS = XPOS - MODR_OGHGT                                         867
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  869
         CALL AGSRESET                                                    869
         AG_X = XPOS-MODR_GRHGT/2                                         869
         AG_TITLE = 'BRGR'                                                869
         AG_EXNAME = 'POSITION'                                           869
         AG_PARLIST = 'X'                                                 869
         CALL AXPOSITION                                                  869
C    *                                                                    869
         XPOS = XPOS - MODR_GRHGT                                         869
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 <===                  871
         CALL AGSRESET                                                    871
         AG_X = XPOS-MODR_MYHGT/2                                         871
         AG_TITLE = 'BRMY'                                                871
         AG_EXNAME = 'POSITION'                                           871
         AG_PARLIST = 'X'                                                 871
         CALL AXPOSITION                                                  871
C    *                                                                    871
         XPOS = XPOS - MODR_MYHGT                                         871
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            873
         CALL AGSRESET                                                    873
         AG_X = XPOS-MODR_PCBHGT/2                                        873
         AG_Z = 0.                                                        873
         AG_TITLE = 'BRCB'                                                873
         AG_EXNAME = 'POSITION'                                           873
         AG_PARLIST = 'X_Z'                                               873
         CALL AXPOSITION                                                  873
C    *                                                                    873
         XPOS = XPOS - MODR_PCBHGT                                        873
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    875
         CALL AGSRESET                                                    875
         AG_X = XPOS-MODR_HCHGT/2                                         875
         AG_Z = MODR_CENTER                                               875
         AG_TITLE = 'BRHC'                                                875
         AG_EXNAME = 'POSITION'                                           875
         AG_PARLIST = 'X_Z'                                               875
         CALL AXPOSITION                                                  875
C    *                                                                    875
         XPOS = XPOS - MODR_HCHGT                                         875
5491  CALL AGSPOPB                                                        877
      GO TO BLOCK_BRMD, (5490 ,5500)                                      877
C End of Local Procedure                                                  877
C ----------------------------------------------------------------------  878
C ------------ BRHC                                                       878
C Begin Local Procedure BRHC                                              879
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                879
5930  CONTINUE                                                            879
         AG_TITLE='BRHC'                                                  879
         AG_MODULE=MODULE                                                 879
         AG_MARK='V00'                                                    879
         CALL AGSPUSH                                                     879
         AG_MODULE=MODULE                                                 879
C    *                                                                    879
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       880
         CALL AGSRESET                                                    880
         AG_SEEN = 1                                                      880
         AG_COLO = 1                                                      880
         AG_TITLE = 'BRHC'                                                880
         AG_EXNAME = 'ATTRIBUTE'                                          880
         AG_PARLIST = 'SEEN_COLO'                                         880
         CALL AXATTRIBUTE                                                 880
C    *                                                                    880
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        880
         CALL AGSRESET                                                    881
         AG_TITLE = 'HONEYCOMB'                                           881
         AG_EXNAME = 'MATERIAL'                                           881
         AG_PARLIST = 'NONE'                                              881
         CALL AXMATERIAL                                                  881
C    *                                                                    881
C    Geant SHAPE : BOX ===> DX=MODR_HCHGT/2 DY=MODR_HCWID/2, DZ=MODR_HCL  881
C    EN/2 <===                                                            881
         CALL AGSRESET                                                    883
         AG_DX = MODR_HCHGT/2                                             883
         AG_DY = MODR_HCWID/2                                             883
         AG_DZ = MODR_HCLEN/2                                             883
         AG_TITLE = 'BOX'                                                 883
         AG_EXNAME = 'SHAPE'                                              883
         AG_PARLIST = 'DX_DY_DZ'                                          883
         CALL AXSHAPE                                                     883
C    *                                                                    883
5931  CALL AGSPOPB                                                        884
      GO TO BLOCK_BRHC, (5930 ,5940)                                      884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ BRCB                                                       885
C Begin Local Procedure BRCB                                              886
C BRCB IS THE PCB IN THE TOFR MODULE                                      886
5950  CONTINUE                                                            886
         AG_TITLE='BRCB'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       887
         CALL AGSRESET                                                    887
         AG_SEEN = 1                                                      887
         AG_COLO = 3                                                      887
         AG_TITLE = 'BRCB'                                                887
         AG_EXNAME = 'ATTRIBUTE'                                          887
         AG_PARLIST = 'SEEN_COLO'                                         887
         CALL AXATTRIBUTE                                                 887
C    *                                                                    887
C    Geant MATERIAL : G10 ===> inherited arguments used <===              887
         CALL AGSRESET                                                    888
         AG_TITLE = 'G10'                                                 888
         AG_EXNAME = 'MATERIAL'                                           888
         AG_PARLIST = 'NONE'                                              888
         CALL AXMATERIAL                                                  888
C    *                                                                    888
C    Geant SHAPE : BOX ===> DX=MODR_PCBHGT/2 DY=MODR_PCBWID/2, DZ=MODR_P  888
C    CBLEN/2 <===                                                         888
         CALL AGSRESET                                                    890
         AG_DX = MODR_PCBHGT/2                                            890
         AG_DY = MODR_PCBWID/2                                            890
         AG_DZ = MODR_PCBLEN/2                                            890
         AG_TITLE = 'BOX'                                                 890
         AG_EXNAME = 'SHAPE'                                              890
         AG_PARLIST = 'DX_DY_DZ'                                          890
         CALL AXSHAPE                                                     890
C    *                                                                    890
5951  CALL AGSPOPB                                                        891
      GO TO BLOCK_BRCB, (5950 ,5960)                                      891
C End of Local Procedure                                                  891
C ----------------------------------------------------------------------  892
C ------------ BRMY                                                       892
C Begin Local Procedure BRMY                                              893
C BRMY IS THE MYLAR IN THE TOFR MODULE                                    893
5970  CONTINUE                                                            893
         AG_TITLE='BRMY'                                                  893
         AG_MODULE=MODULE                                                 893
         AG_MARK='V00'                                                    893
         CALL AGSPUSH                                                     893
         AG_MODULE=MODULE                                                 893
C    *                                                                    893
C    Geant ATTRIBUTE : BRMY ===> SEEN=0 COLO=1 <===                       894
         CALL AGSRESET                                                    894
         AG_SEEN = 0                                                      894
         AG_COLO = 1                                                      894
         AG_TITLE = 'BRMY'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO'                                         894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            894
         CALL AGSRESET                                                    895
         AG_TITLE = 'MYLAR'                                               895
         AG_EXNAME = 'MATERIAL'                                           895
         AG_PARLIST = 'NONE'                                              895
         CALL AXMATERIAL                                                  895
C    *                                                                    895
C    Geant SHAPE : BOX ===> DX=MODR_MYHGT/2 DY=MODR_MYWID/2, DZ=MODR_MYL  895
C    EN/2 <===                                                            895
         CALL AGSRESET                                                    897
         AG_DX = MODR_MYHGT/2                                             897
         AG_DY = MODR_MYWID/2                                             897
         AG_DZ = MODR_MYLEN/2                                             897
         AG_TITLE = 'BOX'                                                 897
         AG_EXNAME = 'SHAPE'                                              897
         AG_PARLIST = 'DX_DY_DZ'                                          897
         CALL AXSHAPE                                                     897
C    *                                                                    897
5971  CALL AGSPOPB                                                        898
      GO TO BLOCK_BRMY, (5970 ,5980)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ BRGR                                                       899
C Begin Local Procedure BRGR                                              900
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 900
5990  CONTINUE                                                            900
         AG_TITLE='BRGR'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       901
         CALL AGSRESET                                                    901
         AG_SEEN = 0                                                      901
         AG_COLO = 1                                                      901
         AG_TITLE = 'BRGR'                                                901
         AG_EXNAME = 'ATTRIBUTE'                                          901
         AG_PARLIST = 'SEEN_COLO'                                         901
         CALL AXATTRIBUTE                                                 901
C    *                                                                    901
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           901
         CALL AGSRESET                                                    902
         AG_TITLE = 'CARBON'                                              902
         AG_EXNAME = 'MATERIAL'                                           902
         AG_PARLIST = 'NONE'                                              902
         CALL AXMATERIAL                                                  902
C    *                                                                    902
C    Geant SHAPE : BOX ===> DX=MODR_GRHGT/2 DY=MODR_GRWID/2, DZ=MODR_GRL  902
C    EN/2 <===                                                            902
         CALL AGSRESET                                                    904
         AG_DX = MODR_GRHGT/2                                             904
         AG_DY = MODR_GRWID/2                                             904
         AG_DZ = MODR_GRLEN/2                                             904
         AG_TITLE = 'BOX'                                                 904
         AG_EXNAME = 'SHAPE'                                              904
         AG_PARLIST = 'DX_DY_DZ'                                          904
         CALL AXSHAPE                                                     904
C    *                                                                    904
5991  CALL AGSPOPB                                                        905
      GO TO BLOCK_BRGR, (5990 ,6000)                                      905
C End of Local Procedure                                                  905
C ----------------------------------------------------------------------  906
C ------------ BROG                                                       906
C Begin Local Procedure BROG                                              907
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              907
6010  CONTINUE                                                            907
         AG_TITLE='BROG'                                                  907
         AG_MODULE=MODULE                                                 907
         AG_MARK='V00'                                                    907
         CALL AGSPUSH                                                     907
         AG_MODULE=MODULE                                                 907
C    *                                                                    907
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       908
         CALL AGSRESET                                                    908
         AG_SEEN = 1                                                      908
         AG_COLO = 7                                                      908
         AG_TITLE = 'BROG'                                                908
         AG_EXNAME = 'ATTRIBUTE'                                          908
         AG_PARLIST = 'SEEN_COLO'                                         908
         CALL AXATTRIBUTE                                                 908
C    *                                                                    908
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        908
         CALL AGSRESET                                                    909
         AG_A = 28                                                        909
         AG_Z = 14                                                        909
         AG_W = 1.                                                        909
         AG_TITLE = 'SI'                                                  909
         AG_EXNAME = 'COMPONENT'                                          909
         AG_PARLIST = 'A_Z_W'                                             909
         CALL AXCOMPONENT                                                 909
C    *                                                                    909
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          909
         CALL AGSRESET                                                    910
         AG_A = 16                                                        910
         AG_Z = 8                                                         910
         AG_W = 2.                                                        910
         AG_TITLE = 'O'                                                   910
         AG_EXNAME = 'COMPONENT'                                          910
         AG_PARLIST = 'A_Z_W'                                             910
         CALL AXCOMPONENT                                                 910
C    *                                                                    910
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             910
         CALL AGSRESET                                                    911
         AG_DENS = 2.2                                                    911
         AG_TITLE = 'GLASS'                                               911
         AG_EXNAME = 'MIXTURE'                                            911
         AG_PARLIST = 'DENS'                                              911
         CALL AXMIXTURE                                                   911
C    *                                                                    911
C    Geant SHAPE : BOX ===> DX=MODR_OGHGT/2 DY=MODR_OGWID/2, DZ=MODR_OGL  911
C    EN/2 <===                                                            911
         CALL AGSRESET                                                    913
         AG_DX = MODR_OGHGT/2                                             913
         AG_DY = MODR_OGWID/2                                             913
         AG_DZ = MODR_OGLEN/2                                             913
         AG_TITLE = 'BOX'                                                 913
         AG_EXNAME = 'SHAPE'                                              913
         AG_PARLIST = 'DX_DY_DZ'                                          913
         CALL AXSHAPE                                                     913
C    *                                                                    913
6011  CALL AGSPOPB                                                        914
      GO TO BLOCK_BROG, (6010 ,6020)                                      914
C End of Local Procedure                                                  914
C ----------------------------------------------------------------------  915
C ------------ BRDT                                                       915
C Begin Local Procedure BRDT                                              916
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     916
6030  CONTINUE                                                            916
         AG_TITLE='BRDT'                                                  916
         AG_MODULE=MODULE                                                 916
         AG_MARK='V00'                                                    916
         CALL AGSPUSH                                                     916
         AG_MODULE=MODULE                                                 916
C    *                                                                    916
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            917
         CALL AGSRESET                                                    917
         AG_TITLE = 'BRDT'                                                917
         AG_EXNAME = 'ATTRIBUTE'                                          917
         AG_PARLIST = 'NONE'                                              917
         CALL AXATTRIBUTE                                                 917
C    *                                                                    917
C    Geant SHAPE : BOX ===> inherited arguments used <===                 917
         CALL AGSRESET                                                    918
         AG_TITLE = 'BOX'                                                 918
         AG_EXNAME = 'SHAPE'                                              918
         AG_PARLIST = 'NONE'                                              918
         CALL AXSHAPE                                                     918
C    *                                                                    918
         AG_KONLY = 'MANY'                                                918
C    Create BRIG :                                                        919
C    Call To Local Procedure BRIG                                         919
         ASSIGN 6080 TO BLOCK_BRIG                                        919
         GO TO  6070                                                      919
6080     CONTINUE                                                         919
C    *                                                                    919
         AG_KONLY = 'MANY'                                                920
C    Create BRSG :                                                        920
C    Call To Local Procedure BRSG                                         920
         ASSIGN 6100 TO BLOCK_BRSG                                        920
         GO TO  6090                                                      920
6100     CONTINUE                                                         920
C    *                                                                    920
         XPOS = DTHGT/2.                                                  921
         Y = 0.                                                           922
         Z = 0.                                                           923
C    *                                                                    924
         IGAP=1                                                           924
            GO TO 6113                                                    924
6111        IGAP=IGAP+1                                                   924
6113        IF(IGAP-(MODR_NGAP-1).GT.0) GO TO 6112                        924
C       Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                925
            CALL AGSRESET                                                 925
            AG_X = XPOS-MODR_SPRMAX                                       925
            AG_TITLE = 'BRSG'                                             925
            AG_EXNAME = 'POSITION'                                        925
            AG_PARLIST = 'X'                                              925
            CALL AXPOSITION                                               925
C       *                                                                 925
            XPOS = XPOS - MODR_SPRMAX*2.                                  925
C       Geant POSITION : BRIG ===> X=XPOS-MODR_IGHGT/2 <===               927
            CALL AGSRESET                                                 927
            AG_X = XPOS-MODR_IGHGT/2                                      927
            AG_TITLE = 'BRIG'                                             927
            AG_EXNAME = 'POSITION'                                        927
            AG_PARLIST = 'X'                                              927
            CALL AXPOSITION                                               927
C       *                                                                 927
            XPOS = XPOS - MODR_IGHGT                                      927
         GO TO 6111                                                       929
6112     CONTINUE                                                         929
C    Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                   930
         CALL AGSRESET                                                    930
         AG_X = XPOS-MODR_SPRMAX                                          930
         AG_TITLE = 'BRSG'                                                930
         AG_EXNAME = 'POSITION'                                           930
         AG_PARLIST = 'X'                                                 930
         CALL AXPOSITION                                                  930
C    *                                                                    930
         XPOS = XPOS - MODR_SPRMAX*2.                                     930
6031  CALL AGSPOPB                                                        932
      GO TO BLOCK_BRDT, (6030 ,6040)                                      932
C End of Local Procedure                                                  932
C ----------------------------------------------------------------------  933
C ------------ BRIG                                                       933
C Begin Local Procedure BRIG                                              934
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              934
6070  CONTINUE                                                            934
         AG_TITLE='BRIG'                                                  934
         AG_MODULE=MODULE                                                 934
         AG_MARK='V00'                                                    934
         CALL AGSPUSH                                                     934
         AG_MODULE=MODULE                                                 934
C    *                                                                    934
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       935
         CALL AGSRESET                                                    935
         AG_SEEN = 1                                                      935
         AG_COLO = 7                                                      935
         AG_TITLE = 'BRIG'                                                935
         AG_EXNAME = 'ATTRIBUTE'                                          935
         AG_PARLIST = 'SEEN_COLO'                                         935
         CALL AXATTRIBUTE                                                 935
C    *                                                                    935
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            935
         CALL AGSRESET                                                    936
         AG_TITLE = 'GLASS'                                               936
         AG_EXNAME = 'MATERIAL'                                           936
         AG_PARLIST = 'NONE'                                              936
         CALL AXMATERIAL                                                  936
C    *                                                                    936
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  936
C    EN/2 <===                                                            936
         CALL AGSRESET                                                    938
         AG_DX = MODR_IGHGT/2                                             938
         AG_DY = MODR_IGWID/2                                             938
         AG_DZ = MODR_IGLEN/2                                             938
         AG_TITLE = 'BOX'                                                 938
         AG_EXNAME = 'SHAPE'                                              938
         AG_PARLIST = 'DX_DY_DZ'                                          938
         CALL AXSHAPE                                                     938
C    *                                                                    938
6071  CALL AGSPOPB                                                        939
      GO TO BLOCK_BRIG, (6070 ,6080)                                      939
C End of Local Procedure                                                  939
C ----------------------------------------------------------------------  940
C ------------ BRSG                                                       940
C Begin Local Procedure BRSG                                              941
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      941
6090  CONTINUE                                                            941
         AG_TITLE='BRSG'                                                  941
         AG_MODULE=MODULE                                                 941
         AG_MARK='V00'                                                    941
         CALL AGSPUSH                                                     941
         AG_MODULE=MODULE                                                 941
C    *                                                                    941
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       942
         CALL AGSRESET                                                    942
         AG_SEEN = 0                                                      942
         AG_COLO = 5                                                      942
         AG_TITLE = 'BRSG'                                                942
         AG_EXNAME = 'ATTRIBUTE'                                          942
         AG_PARLIST = 'SEEN_COLO'                                         942
         CALL AXATTRIBUTE                                                 942
C    *                                                                    942
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           942
         CALL AGSRESET                                                    943
         AG_TITLE = 'RPCGAS'                                              943
         AG_EXNAME = 'MATERIAL'                                           943
         AG_PARLIST = 'NONE'                                              943
         CALL AXMATERIAL                                                  943
C    *                                                                    943
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           943
         CALL AGSRESET                                                    944
         AG_ISVOL = 1                                                     944
         AG_TITLE = 'SENSITIVE'                                           944
         AG_EXNAME = 'MEDIUM'                                             944
         AG_PARLIST = 'ISVOL'                                             944
         CALL AXMEDIUM                                                    944
C    *                                                                    944
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  944
C    EN/2 <===                                                            944
         CALL AGSRESET                                                    946
         AG_DX = MODR_IGHGT/2                                             946
         AG_DY = MODR_IGWID/2                                             946
         AG_DZ = MODR_IGLEN/2                                             946
         AG_TITLE = 'BOX'                                                 946
         AG_EXNAME = 'SHAPE'                                              946
         AG_PARLIST = 'DX_DY_DZ'                                          946
         CALL AXSHAPE                                                     946
C    *                                                                    946
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  946
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    946
         AG_TITLE = 'BRSG'                                                948
         AG_EXNAME = 'HITS'                                               948
         AG_TYPE=AG_MAGIC                                                 948
         CALL AGSDETV('BRSG')                                             948
C    *                                                                    948
         AG_PARLIST='X'                                                   948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = AG_MAGIC                                              948
         AG_BIN=0.01                                                      948
         AG_OPTION='HS'                                                   948
         AG_FHMIN = AG_MAGIC                                              948
         AG_FHMAX = AG_MAGIC                                              948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='Y'                                                   948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = AG_MAGIC                                              948
         AG_BIN=0.01                                                      948
         AG_FHMIN=AG_MAGIC                                                948
         AG_FHMAX=AG_MAGIC                                                948
         AG_OPTION='*'                                                    948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='Z'                                                   948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = AG_MAGIC                                              948
         AG_BIN=0.01                                                      948
         AG_FHMIN=AG_MAGIC                                                948
         AG_FHMAX=AG_MAGIC                                                948
         AG_OPTION='*'                                                    948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='PTOT'                                                948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = 18                                                    948
         AG_BIN=AG_MAGIC                                                  948
         AG_OPTION='*'                                                    948
         AG_FHMIN = 0.0                                                   948
         AG_FHMAX = 100.0                                                 948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='SLENG'                                               948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = AG_MAGIC                                              948
         AG_BIN=0.1                                                       948
         AG_OPTION='*'                                                    948
         AG_FHMIN = 0                                                     948
         AG_FHMAX = 500                                                   948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='TOF'                                                 948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = 16                                                    948
         AG_BIN=AG_MAGIC                                                  948
         AG_OPTION='*'                                                    948
         AG_FHMIN = 0.0                                                   948
         AG_FHMAX = 1.0E-7                                                948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='STEP'                                                948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = AG_MAGIC                                              948
         AG_BIN=0.01                                                      948
         AG_FHMIN=AG_MAGIC                                                948
         AG_FHMAX=AG_MAGIC                                                948
         AG_OPTION='*'                                                    948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         AG_PARLIST='ELOSS'                                               948
         AG_TITLE = 'DETH'                                                948
         AG_NBITS = 16                                                    948
         AG_BIN=AG_MAGIC                                                  948
         AG_OPTION='*'                                                    948
         AG_FHMIN = 0.0                                                   948
         AG_FHMAX = 1.0E-6                                                948
         CALL AGSDETH (0)                                                 948
C    *                                                                    948
         CALL AGSDETU('BRSG')                                             948
C    *                                                                    948
6091  CALL AGSPOPB                                                        949
      GO TO BLOCK_BRSG, (6090 ,6100)                                      949
C End of Local Procedure                                                  949
C ----------------------------------------------------------------------  950
C ------------ BRWG                                                       950
C Begin Local Procedure BRWG                                              951
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           951
6050  CONTINUE                                                            951
         AG_TITLE='BRWG'                                                  951
         AG_MODULE=MODULE                                                 951
         AG_MARK='V00'                                                    951
         CALL AGSPUSH                                                     951
         AG_MODULE=MODULE                                                 951
C    *                                                                    951
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              952
         CALL AGSRESET                                                    952
         AG_SEEN = 0                                                      952
         AG_TITLE = 'BRWG'                                                952
         AG_EXNAME = 'ATTRIBUTE'                                          952
         AG_PARLIST = 'SEEN'                                              952
         CALL AXATTRIBUTE                                                 952
C    *                                                                    952
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           952
         CALL AGSRESET                                                    953
         AG_A = 16                                                        953
         AG_Z = 8                                                         953
         AG_W = 2                                                         953
         AG_TITLE = 'O'                                                   953
         AG_EXNAME = 'COMPONENT'                                          953
         AG_PARLIST = 'A_Z_W'                                             953
         CALL AXCOMPONENT                                                 953
C    *                                                                    953
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           953
         CALL AGSRESET                                                    954
         AG_A = 12                                                        954
         AG_Z = 6                                                         954
         AG_W = 5                                                         954
         AG_TITLE = 'C'                                                   954
         AG_EXNAME = 'COMPONENT'                                          954
         AG_PARLIST = 'A_Z_W'                                             954
         CALL AXCOMPONENT                                                 954
C    *                                                                    954
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            954
         CALL AGSRESET                                                    955
         AG_A = 1                                                         955
         AG_Z = 1                                                         955
         AG_W = 8                                                         955
         AG_TITLE = 'H'                                                   955
         AG_EXNAME = 'COMPONENT'                                          955
         AG_PARLIST = 'A_Z_W'                                             955
         CALL AXCOMPONENT                                                 955
C    *                                                                    955
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           955
         CALL AGSRESET                                                    956
         AG_DENS = 1.18                                                   956
         AG_TITLE = 'LUCITE'                                              956
         AG_EXNAME = 'MIXTURE'                                            956
         AG_PARLIST = 'DENS'                                              956
         CALL AXMIXTURE                                                   956
C    *                                                                    956
C    Geant SHAPE : TUBE ===> RMIN=MODR_WGRMIN RMAX=MODR_WGRMAX, DZ=MODR_  956
C    WGLEN/2 <===                                                         956
         CALL AGSRESET                                                    958
         AG_RMIN = MODR_WGRMIN                                            958
         AG_RMAX = MODR_WGRMAX                                            958
         AG_DZ = MODR_WGLEN/2                                             958
         AG_TITLE = 'TUBE'                                                958
         AG_EXNAME = 'SHAPE'                                              958
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      958
         CALL AXSHAPE                                                     958
C    *                                                                    958
6051  CALL AGSPOPB                                                        959
      GO TO BLOCK_BRWG, (6050 ,6060)                                      959
C End of Local Procedure                                                  959
      WRITE(LOUT,6120)'===>>  [End AgML Module BTOFGEO4]  <<==='          959
6120  FORMAT(A32,/,/)                                                     959
C MODULE BTOFGEO4                                                         961
      END                                                                 961
