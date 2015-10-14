C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO5                                                 1
C BTOFGEO5 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO5'/, BANK_TITLE/'*'/                            1
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
      REAL BTOG_POSIT4(5)                                                 11
      EQUIVALENCE (BANK_BTOG(12),BTOG_POSIT4,RE_BTOG_POSIT4),             11
     *(NAMES_BTOG(9),NBTOG_POSIT4)                                        11
      EQUIVALENCE (COMMEN_BTOG(9),CBTOG_POSIT4),(MAP_BTOG(1,9),           11
     *MBTOG_POSIT4)                                                       11
      CHARACTER CBTOG_POSIT4*40/' '/                                      11
      CHARACTER NBTOG_POSIT4*12/'R.POSIT4'/                               11
      INTEGER MBTOG_POSIT4(2)/5,1/                                        11
      REAL RE_BTOG_POSIT4                                                 11
C -                                                                       11
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                11
      PARAMETER (LEN_BTOG=16)                                             11
      PARAMETER (LLEN_BTOG=9)                                             11
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
C -                                                                       23
      REAL TOFF_BOXWIDTH                                                  23
      EQUIVALENCE (BANK_TOFF(3),TOFF_BOXWIDTH,RE_TOFF_BOXWIDTH),          23
     *(NAMES_TOFF(1),NTOFF_BOXWIDTH)                                      23
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_BOXWIDTH),(MAP_TOFF(1,1),         23
     *MTOFF_BOXWIDTH)                                                     23
      CHARACTER CTOFF_BOXWIDTH*40/' '/                                    23
      CHARACTER NTOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           23
      INTEGER MTOFF_BOXWIDTH(2)/1,1/                                      23
      REAL RE_TOFF_BOXWIDTH                                               23
C -                                                                       23
      REAL TOFF_SLATLEN                                                   23
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLATLEN,RE_TOFF_SLATLEN),            23
     *(NAMES_TOFF(2),NTOFF_SLATLEN)                                       23
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLATLEN),(MAP_TOFF(1,2),          23
     *MTOFF_SLATLEN)                                                      23
      CHARACTER CTOFF_SLATLEN*40/' '/                                     23
      CHARACTER NTOFF_SLATLEN*12/'R.SLATLEN'/                             23
      INTEGER MTOFF_SLATLEN(2)/1,1/                                       23
      REAL RE_TOFF_SLATLEN                                                23
C -                                                                       23
      REAL TOFF_SLAT5Z                                                    23
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLAT5Z,RE_TOFF_SLAT5Z),              23
     *(NAMES_TOFF(3),NTOFF_SLAT5Z)                                        23
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLAT5Z),(MAP_TOFF(1,3),           23
     *MTOFF_SLAT5Z)                                                       23
      CHARACTER CTOFF_SLAT5Z*40/' '/                                      23
      CHARACTER NTOFF_SLAT5Z*12/'R.SLAT5Z'/                               23
      INTEGER MTOFF_SLAT5Z(2)/1,1/                                        23
      REAL RE_TOFF_SLAT5Z                                                 23
C -                                                                       23
      REAL TOFF_SLATZ(10)                                                 23
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLATZ,RE_TOFF_SLATZ),                23
     *(NAMES_TOFF(4),NTOFF_SLATZ)                                         23
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLATZ),(MAP_TOFF(1,4),            23
     *MTOFF_SLATZ)                                                        23
      CHARACTER CTOFF_SLATZ*40/' '/                                       23
      CHARACTER NTOFF_SLATZ*12/'R.SLATZ'/                                 23
      INTEGER MTOFF_SLATZ(2)/10,1/                                        23
      REAL RE_TOFF_SLATZ                                                  23
C -                                                                       23
      REAL TOFF_SLATX(10)                                                 23
      EQUIVALENCE (BANK_TOFF(16),TOFF_SLATX,RE_TOFF_SLATX),               23
     *(NAMES_TOFF(5),NTOFF_SLATX)                                         23
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLATX),(MAP_TOFF(1,5),            23
     *MTOFF_SLATX)                                                        23
      CHARACTER CTOFF_SLATX*40/' '/                                       23
      CHARACTER NTOFF_SLATX*12/'R.SLATX'/                                 23
      INTEGER MTOFF_SLATX(2)/10,1/                                        23
      REAL RE_TOFF_SLATX                                                  23
C -                                                                       23
      REAL TOFF_SLATAY(10)                                                23
      EQUIVALENCE (BANK_TOFF(26),TOFF_SLATAY,RE_TOFF_SLATAY),             23
     *(NAMES_TOFF(6),NTOFF_SLATAY)                                        23
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLATAY),(MAP_TOFF(1,6),           23
     *MTOFF_SLATAY)                                                       23
      CHARACTER CTOFF_SLATAY*40/' '/                                      23
      CHARACTER NTOFF_SLATAY*12/'R.SLATAY'/                               23
      INTEGER MTOFF_SLATAY(2)/10,1/                                       23
      REAL RE_TOFF_SLATAY                                                 23
C -                                                                       23
      REAL TOFF_SLATTHCK                                                  23
      EQUIVALENCE (BANK_TOFF(36),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),         23
     *(NAMES_TOFF(7),NTOFF_SLATTHCK)                                      23
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_SLATTHCK),(MAP_TOFF(1,7),         23
     *MTOFF_SLATTHCK)                                                     23
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    23
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           23
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      23
      REAL RE_TOFF_SLATTHCK                                               23
C -                                                                       23
      REAL TOFF_SLATWID                                                   23
      EQUIVALENCE (BANK_TOFF(37),TOFF_SLATWID,RE_TOFF_SLATWID),           23
     *(NAMES_TOFF(8),NTOFF_SLATWID)                                       23
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_SLATWID),(MAP_TOFF(1,8),          23
     *MTOFF_SLATWID)                                                      23
      CHARACTER CTOFF_SLATWID*40/' '/                                     23
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             23
      INTEGER MTOFF_SLATWID(2)/1,1/                                       23
      REAL RE_TOFF_SLATWID                                                23
C -                                                                       23
      REAL TOFF_SLATANG                                                   23
      EQUIVALENCE (BANK_TOFF(38),TOFF_SLATANG,RE_TOFF_SLATANG),           23
     *(NAMES_TOFF(9),NTOFF_SLATANG)                                       23
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_SLATANG),(MAP_TOFF(1,9),          23
     *MTOFF_SLATANG)                                                      23
      CHARACTER CTOFF_SLATANG*40/' '/                                     23
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             23
      INTEGER MTOFF_SLATANG(2)/1,1/                                       23
      REAL RE_TOFF_SLATANG                                                23
C -                                                                       23
      REAL TOFF_PMTLEN                                                    23
      EQUIVALENCE (BANK_TOFF(39),TOFF_PMTLEN,RE_TOFF_PMTLEN),             23
     *(NAMES_TOFF(10),NTOFF_PMTLEN)                                       23
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_PMTLEN),(MAP_TOFF(1,10),         23
     *MTOFF_PMTLEN)                                                       23
      CHARACTER CTOFF_PMTLEN*40/' '/                                      23
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               23
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        23
      REAL RE_TOFF_PMTLEN                                                 23
C -                                                                       23
      REAL TOFF_PMTMAXR                                                   23
      EQUIVALENCE (BANK_TOFF(40),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           23
     *(NAMES_TOFF(11),NTOFF_PMTMAXR)                                      23
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_PMTMAXR),(MAP_TOFF(1,11),        23
     *MTOFF_PMTMAXR)                                                      23
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     23
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             23
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       23
      REAL RE_TOFF_PMTMAXR                                                23
C -                                                                       23
      REAL TOFF_PMTMINR                                                   23
      EQUIVALENCE (BANK_TOFF(41),TOFF_PMTMINR,RE_TOFF_PMTMINR),           23
     *(NAMES_TOFF(12),NTOFF_PMTMINR)                                      23
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_PMTMINR),(MAP_TOFF(1,12),        23
     *MTOFF_PMTMINR)                                                      23
      CHARACTER CTOFF_PMTMINR*40/' '/                                     23
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             23
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       23
      REAL RE_TOFF_PMTMINR                                                23
C -                                                                       23
      REAL TOFF_BASELEN                                                   23
      EQUIVALENCE (BANK_TOFF(42),TOFF_BASELEN,RE_TOFF_BASELEN),           23
     *(NAMES_TOFF(13),NTOFF_BASELEN)                                      23
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_BASELEN),(MAP_TOFF(1,13),        23
     *MTOFF_BASELEN)                                                      23
      CHARACTER CTOFF_BASELEN*40/' '/                                     23
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             23
      INTEGER MTOFF_BASELEN(2)/1,1/                                       23
      REAL RE_TOFF_BASELEN                                                23
C -                                                                       23
      REAL TOFF_BASEMAXR                                                  23
      EQUIVALENCE (BANK_TOFF(43),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         23
     *(NAMES_TOFF(14),NTOFF_BASEMAXR)                                     23
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_BASEMAXR),(MAP_TOFF(1,14),       23
     *MTOFF_BASEMAXR)                                                     23
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    23
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           23
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      23
      REAL RE_TOFF_BASEMAXR                                               23
C -                                                                       23
      REAL TOFF_BASEMINR                                                  23
      EQUIVALENCE (BANK_TOFF(44),TOFF_BASEMINR,RE_TOFF_BASEMINR),         23
     *(NAMES_TOFF(15),NTOFF_BASEMINR)                                     23
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_BASEMINR),(MAP_TOFF(1,15),       23
     *MTOFF_BASEMINR)                                                     23
      CHARACTER CTOFF_BASEMINR*40/' '/                                    23
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           23
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      23
      REAL RE_TOFF_BASEMINR                                               23
C -                                                                       23
      REAL TOFF_SOCKLEN                                                   23
      EQUIVALENCE (BANK_TOFF(45),TOFF_SOCKLEN,RE_TOFF_SOCKLEN),           23
     *(NAMES_TOFF(16),NTOFF_SOCKLEN)                                      23
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_SOCKLEN),(MAP_TOFF(1,16),        23
     *MTOFF_SOCKLEN)                                                      23
      CHARACTER CTOFF_SOCKLEN*40/' '/                                     23
      CHARACTER NTOFF_SOCKLEN*12/'R.SOCKLEN'/                             23
      INTEGER MTOFF_SOCKLEN(2)/1,1/                                       23
      REAL RE_TOFF_SOCKLEN                                                23
C -                                                                       23
      REAL TOFF_CELLWID                                                   23
      EQUIVALENCE (BANK_TOFF(46),TOFF_CELLWID,RE_TOFF_CELLWID),           23
     *(NAMES_TOFF(17),NTOFF_CELLWID)                                      23
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_CELLWID),(MAP_TOFF(1,17),        23
     *MTOFF_CELLWID)                                                      23
      CHARACTER CTOFF_CELLWID*40/' '/                                     23
      CHARACTER NTOFF_CELLWID*12/'R.CELLWID'/                             23
      INTEGER MTOFF_CELLWID(2)/1,1/                                       23
      REAL RE_TOFF_CELLWID                                                23
C -                                                                       23
      REAL TOFF_CELLHGT                                                   23
      EQUIVALENCE (BANK_TOFF(47),TOFF_CELLHGT,RE_TOFF_CELLHGT),           23
     *(NAMES_TOFF(18),NTOFF_CELLHGT)                                      23
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_CELLHGT),(MAP_TOFF(1,18),        23
     *MTOFF_CELLHGT)                                                      23
      CHARACTER CTOFF_CELLHGT*40/' '/                                     23
      CHARACTER NTOFF_CELLHGT*12/'R.CELLHGT'/                             23
      INTEGER MTOFF_CELLHGT(2)/1,1/                                       23
      REAL RE_TOFF_CELLHGT                                                23
C -                                                                       23
      REAL TOFF_ELECHGT                                                   23
      EQUIVALENCE (BANK_TOFF(48),TOFF_ELECHGT,RE_TOFF_ELECHGT),           23
     *(NAMES_TOFF(19),NTOFF_ELECHGT)                                      23
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_ELECHGT),(MAP_TOFF(1,19),        23
     *MTOFF_ELECHGT)                                                      23
      CHARACTER CTOFF_ELECHGT*40/' '/                                     23
      CHARACTER NTOFF_ELECHGT*12/'R.ELECHGT'/                             23
      INTEGER MTOFF_ELECHGT(2)/1,1/                                       23
      REAL RE_TOFF_ELECHGT                                                23
C -                                                                       23
      REAL TOFF_ELECTHCK                                                  23
      EQUIVALENCE (BANK_TOFF(49),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         23
     *(NAMES_TOFF(20),NTOFF_ELECTHCK)                                     23
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_ELECTHCK),(MAP_TOFF(1,20),       23
     *MTOFF_ELECTHCK)                                                     23
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    23
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           23
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      23
      REAL RE_TOFF_ELECTHCK                                               23
C -                                                                       23
      REAL TOFF_ELECWID                                                   23
      EQUIVALENCE (BANK_TOFF(50),TOFF_ELECWID,RE_TOFF_ELECWID),           23
     *(NAMES_TOFF(21),NTOFF_ELECWID)                                      23
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_ELECWID),(MAP_TOFF(1,21),        23
     *MTOFF_ELECWID)                                                      23
      CHARACTER CTOFF_ELECWID*40/' '/                                     23
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             23
      INTEGER MTOFF_ELECWID(2)/1,1/                                       23
      REAL RE_TOFF_ELECWID                                                23
C -                                                                       23
      REAL TOFF_ELECLEN                                                   23
      EQUIVALENCE (BANK_TOFF(51),TOFF_ELECLEN,RE_TOFF_ELECLEN),           23
     *(NAMES_TOFF(22),NTOFF_ELECLEN)                                      23
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_ELECLEN),(MAP_TOFF(1,22),        23
     *MTOFF_ELECLEN)                                                      23
      CHARACTER CTOFF_ELECLEN*40/' '/                                     23
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             23
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       23
      REAL RE_TOFF_ELECLEN                                                23
C -                                                                       23
      REAL TOFF_ELECZ(10)                                                 23
      EQUIVALENCE (BANK_TOFF(52),TOFF_ELECZ,RE_TOFF_ELECZ),               23
     *(NAMES_TOFF(23),NTOFF_ELECZ)                                        23
      EQUIVALENCE (COMMEN_TOFF(23),CTOFF_ELECZ),(MAP_TOFF(1,23),          23
     *MTOFF_ELECZ)                                                        23
      CHARACTER CTOFF_ELECZ*40/' '/                                       23
      CHARACTER NTOFF_ELECZ*12/'R.ELECZ'/                                 23
      INTEGER MTOFF_ELECZ(2)/10,1/                                        23
      REAL RE_TOFF_ELECZ                                                  23
C -                                                                       23
      REAL TOFF_PLASPOS                                                   23
      EQUIVALENCE (BANK_TOFF(62),TOFF_PLASPOS,RE_TOFF_PLASPOS),           23
     *(NAMES_TOFF(24),NTOFF_PLASPOS)                                      23
      EQUIVALENCE (COMMEN_TOFF(24),CTOFF_PLASPOS),(MAP_TOFF(1,24),        23
     *MTOFF_PLASPOS)                                                      23
      CHARACTER CTOFF_PLASPOS*40/' '/                                     23
      CHARACTER NTOFF_PLASPOS*12/'R.PLASPOS'/                             23
      INTEGER MTOFF_PLASPOS(2)/1,1/                                       23
      REAL RE_TOFF_PLASPOS                                                23
C -                                                                       23
      REAL TOFF_RAILTHCK                                                  23
      EQUIVALENCE (BANK_TOFF(63),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         23
     *(NAMES_TOFF(25),NTOFF_RAILTHCK)                                     23
      EQUIVALENCE (COMMEN_TOFF(25),CTOFF_RAILTHCK),(MAP_TOFF(1,25),       23
     *MTOFF_RAILTHCK)                                                     23
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    23
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           23
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      23
      REAL RE_TOFF_RAILTHCK                                               23
C -                                                                       23
      REAL TOFF_RAILWID                                                   23
      EQUIVALENCE (BANK_TOFF(64),TOFF_RAILWID,RE_TOFF_RAILWID),           23
     *(NAMES_TOFF(26),NTOFF_RAILWID)                                      23
      EQUIVALENCE (COMMEN_TOFF(26),CTOFF_RAILWID),(MAP_TOFF(1,26),        23
     *MTOFF_RAILWID)                                                      23
      CHARACTER CTOFF_RAILWID*40/' '/                                     23
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             23
      INTEGER MTOFF_RAILWID(2)/1,1/                                       23
      REAL RE_TOFF_RAILWID                                                23
C -                                                                       23
      REAL TOFF_COOLINNR                                                  23
      EQUIVALENCE (BANK_TOFF(65),TOFF_COOLINNR,RE_TOFF_COOLINNR),         23
     *(NAMES_TOFF(27),NTOFF_COOLINNR)                                     23
      EQUIVALENCE (COMMEN_TOFF(27),CTOFF_COOLINNR),(MAP_TOFF(1,27),       23
     *MTOFF_COOLINNR)                                                     23
      CHARACTER CTOFF_COOLINNR*40/' '/                                    23
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           23
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      23
      REAL RE_TOFF_COOLINNR                                               23
C -                                                                       23
      REAL TOFF_COOLOUTR                                                  23
      EQUIVALENCE (BANK_TOFF(66),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         23
     *(NAMES_TOFF(28),NTOFF_COOLOUTR)                                     23
      EQUIVALENCE (COMMEN_TOFF(28),CTOFF_COOLOUTR),(MAP_TOFF(1,28),       23
     *MTOFF_COOLOUTR)                                                     23
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    23
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           23
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      23
      REAL RE_TOFF_COOLOUTR                                               23
C -                                                                       23
      REAL TOFF_BCONYLEN                                                  23
      EQUIVALENCE (BANK_TOFF(67),TOFF_BCONYLEN,RE_TOFF_BCONYLEN),         23
     *(NAMES_TOFF(29),NTOFF_BCONYLEN)                                     23
      EQUIVALENCE (COMMEN_TOFF(29),CTOFF_BCONYLEN),(MAP_TOFF(1,29),       23
     *MTOFF_BCONYLEN)                                                     23
      CHARACTER CTOFF_BCONYLEN*40/' '/                                    23
      CHARACTER NTOFF_BCONYLEN*12/'R.BCONYLEN'/                           23
      INTEGER MTOFF_BCONYLEN(2)/1,1/                                      23
      REAL RE_TOFF_BCONYLEN                                               23
C -                                                                       23
      REAL TOFF_BCONZLEN                                                  23
      EQUIVALENCE (BANK_TOFF(68),TOFF_BCONZLEN,RE_TOFF_BCONZLEN),         23
     *(NAMES_TOFF(30),NTOFF_BCONZLEN)                                     23
      EQUIVALENCE (COMMEN_TOFF(30),CTOFF_BCONZLEN),(MAP_TOFF(1,30),       23
     *MTOFF_BCONZLEN)                                                     23
      CHARACTER CTOFF_BCONZLEN*40/' '/                                    23
      CHARACTER NTOFF_BCONZLEN*12/'R.BCONZLEN'/                           23
      INTEGER MTOFF_BCONZLEN(2)/1,1/                                      23
      REAL RE_TOFF_BCONZLEN                                               23
C -                                                                       23
      REAL TOFF_BCONPLDX                                                  23
      EQUIVALENCE (BANK_TOFF(69),TOFF_BCONPLDX,RE_TOFF_BCONPLDX),         23
     *(NAMES_TOFF(31),NTOFF_BCONPLDX)                                     23
      EQUIVALENCE (COMMEN_TOFF(31),CTOFF_BCONPLDX),(MAP_TOFF(1,31),       23
     *MTOFF_BCONPLDX)                                                     23
      CHARACTER CTOFF_BCONPLDX*40/' '/                                    23
      CHARACTER NTOFF_BCONPLDX*12/'R.BCONPLDX'/                           23
      INTEGER MTOFF_BCONPLDX(2)/1,1/                                      23
      REAL RE_TOFF_BCONPLDX                                               23
C -                                                                       23
      REAL TOFF_BCONPLDZ                                                  23
      EQUIVALENCE (BANK_TOFF(70),TOFF_BCONPLDZ,RE_TOFF_BCONPLDZ),         23
     *(NAMES_TOFF(32),NTOFF_BCONPLDZ)                                     23
      EQUIVALENCE (COMMEN_TOFF(32),CTOFF_BCONPLDZ),(MAP_TOFF(1,32),       23
     *MTOFF_BCONPLDZ)                                                     23
      CHARACTER CTOFF_BCONPLDZ*40/' '/                                    23
      CHARACTER NTOFF_BCONPLDZ*12/'R.BCONPLDZ'/                           23
      INTEGER MTOFF_BCONPLDZ(2)/1,1/                                      23
      REAL RE_TOFF_BCONPLDZ                                               23
C -                                                                       23
      REAL TOFF_BLEMPOSX                                                  23
      EQUIVALENCE (BANK_TOFF(71),TOFF_BLEMPOSX,RE_TOFF_BLEMPOSX),         23
     *(NAMES_TOFF(33),NTOFF_BLEMPOSX)                                     23
      EQUIVALENCE (COMMEN_TOFF(33),CTOFF_BLEMPOSX),(MAP_TOFF(1,33),       23
     *MTOFF_BLEMPOSX)                                                     23
      CHARACTER CTOFF_BLEMPOSX*40/' '/                                    23
      CHARACTER NTOFF_BLEMPOSX*12/'R.BLEMPOSX'/                           23
      INTEGER MTOFF_BLEMPOSX(2)/1,1/                                      23
      REAL RE_TOFF_BLEMPOSX                                               23
C -                                                                       23
      REAL TOFF_BLEMPOSY(15)                                              23
      EQUIVALENCE (BANK_TOFF(72),TOFF_BLEMPOSY,RE_TOFF_BLEMPOSY),         23
     *(NAMES_TOFF(34),NTOFF_BLEMPOSY)                                     23
      EQUIVALENCE (COMMEN_TOFF(34),CTOFF_BLEMPOSY),(MAP_TOFF(1,34),       23
     *MTOFF_BLEMPOSY)                                                     23
      CHARACTER CTOFF_BLEMPOSY*40/' '/                                    23
      CHARACTER NTOFF_BLEMPOSY*12/'R.BLEMPOSY'/                           23
      INTEGER MTOFF_BLEMPOSY(2)/15,1/                                     23
      REAL RE_TOFF_BLEMPOSY                                               23
C -                                                                       23
      REAL TOFF_BLEMPOSZ1                                                 23
      EQUIVALENCE (BANK_TOFF(87),TOFF_BLEMPOSZ1,RE_TOFF_BLEMPOSZ1),       23
     *(NAMES_TOFF(35),NTOFF_BLEMPOSZ1)                                    23
      EQUIVALENCE (COMMEN_TOFF(35),CTOFF_BLEMPOSZ1),(MAP_TOFF(1,35),      23
     *MTOFF_BLEMPOSZ1)                                                    23
      CHARACTER CTOFF_BLEMPOSZ1*40/' '/                                   23
      CHARACTER NTOFF_BLEMPOSZ1*12/'R.BLEMPOSZ1'/                         23
      INTEGER MTOFF_BLEMPOSZ1(2)/1,1/                                     23
      REAL RE_TOFF_BLEMPOSZ1                                              23
C -                                                                       23
      REAL TOFF_BLEMPOSZ2                                                 23
      EQUIVALENCE (BANK_TOFF(88),TOFF_BLEMPOSZ2,RE_TOFF_BLEMPOSZ2),       23
     *(NAMES_TOFF(36),NTOFF_BLEMPOSZ2)                                    23
      EQUIVALENCE (COMMEN_TOFF(36),CTOFF_BLEMPOSZ2),(MAP_TOFF(1,36),      23
     *MTOFF_BLEMPOSZ2)                                                    23
      CHARACTER CTOFF_BLEMPOSZ2*40/' '/                                   23
      CHARACTER NTOFF_BLEMPOSZ2*12/'R.BLEMPOSZ2'/                         23
      INTEGER MTOFF_BLEMPOSZ2(2)/1,1/                                     23
      REAL RE_TOFF_BLEMPOSZ2                                              23
C -                                                                       23
      REAL TOFF_BLEMLENX                                                  23
      EQUIVALENCE (BANK_TOFF(89),TOFF_BLEMLENX,RE_TOFF_BLEMLENX),         23
     *(NAMES_TOFF(37),NTOFF_BLEMLENX)                                     23
      EQUIVALENCE (COMMEN_TOFF(37),CTOFF_BLEMLENX),(MAP_TOFF(1,37),       23
     *MTOFF_BLEMLENX)                                                     23
      CHARACTER CTOFF_BLEMLENX*40/' '/                                    23
      CHARACTER NTOFF_BLEMLENX*12/'R.BLEMLENX'/                           23
      INTEGER MTOFF_BLEMLENX(2)/1,1/                                      23
      REAL RE_TOFF_BLEMLENX                                               23
C -                                                                       23
      REAL TOFF_BLEMLENY                                                  23
      EQUIVALENCE (BANK_TOFF(90),TOFF_BLEMLENY,RE_TOFF_BLEMLENY),         23
     *(NAMES_TOFF(38),NTOFF_BLEMLENY)                                     23
      EQUIVALENCE (COMMEN_TOFF(38),CTOFF_BLEMLENY),(MAP_TOFF(1,38),       23
     *MTOFF_BLEMLENY)                                                     23
      CHARACTER CTOFF_BLEMLENY*40/' '/                                    23
      CHARACTER NTOFF_BLEMLENY*12/'R.BLEMLENY'/                           23
      INTEGER MTOFF_BLEMLENY(2)/1,1/                                      23
      REAL RE_TOFF_BLEMLENY                                               23
C -                                                                       23
      REAL TOFF_BLEMLENZ                                                  23
      EQUIVALENCE (BANK_TOFF(91),TOFF_BLEMLENZ,RE_TOFF_BLEMLENZ),         23
     *(NAMES_TOFF(39),NTOFF_BLEMLENZ)                                     23
      EQUIVALENCE (COMMEN_TOFF(39),CTOFF_BLEMLENZ),(MAP_TOFF(1,39),       23
     *MTOFF_BLEMLENZ)                                                     23
      CHARACTER CTOFF_BLEMLENZ*40/' '/                                    23
      CHARACTER NTOFF_BLEMLENZ*12/'R.BLEMLENZ'/                           23
      INTEGER MTOFF_BLEMLENZ(2)/1,1/                                      23
      REAL RE_TOFF_BLEMLENZ                                               23
C -                                                                       23
      REAL TOFF_BPIPPOSX                                                  23
      EQUIVALENCE (BANK_TOFF(92),TOFF_BPIPPOSX,RE_TOFF_BPIPPOSX),         23
     *(NAMES_TOFF(40),NTOFF_BPIPPOSX)                                     23
      EQUIVALENCE (COMMEN_TOFF(40),CTOFF_BPIPPOSX),(MAP_TOFF(1,40),       23
     *MTOFF_BPIPPOSX)                                                     23
      CHARACTER CTOFF_BPIPPOSX*40/' '/                                    23
      CHARACTER NTOFF_BPIPPOSX*12/'R.BPIPPOSX'/                           23
      INTEGER MTOFF_BPIPPOSX(2)/1,1/                                      23
      REAL RE_TOFF_BPIPPOSX                                               23
C -                                                                       23
      REAL TOFF_BPIPPOSY                                                  23
      EQUIVALENCE (BANK_TOFF(93),TOFF_BPIPPOSY,RE_TOFF_BPIPPOSY),         23
     *(NAMES_TOFF(41),NTOFF_BPIPPOSY)                                     23
      EQUIVALENCE (COMMEN_TOFF(41),CTOFF_BPIPPOSY),(MAP_TOFF(1,41),       23
     *MTOFF_BPIPPOSY)                                                     23
      CHARACTER CTOFF_BPIPPOSY*40/' '/                                    23
      CHARACTER NTOFF_BPIPPOSY*12/'R.BPIPPOSY'/                           23
      INTEGER MTOFF_BPIPPOSY(2)/1,1/                                      23
      REAL RE_TOFF_BPIPPOSY                                               23
C -                                                                       23
      REAL TOFF_BPIPPOSZ                                                  23
      EQUIVALENCE (BANK_TOFF(94),TOFF_BPIPPOSZ,RE_TOFF_BPIPPOSZ),         23
     *(NAMES_TOFF(42),NTOFF_BPIPPOSZ)                                     23
      EQUIVALENCE (COMMEN_TOFF(42),CTOFF_BPIPPOSZ),(MAP_TOFF(1,42),       23
     *MTOFF_BPIPPOSZ)                                                     23
      CHARACTER CTOFF_BPIPPOSZ*40/' '/                                    23
      CHARACTER NTOFF_BPIPPOSZ*12/'R.BPIPPOSZ'/                           23
      INTEGER MTOFF_BPIPPOSZ(2)/1,1/                                      23
      REAL RE_TOFF_BPIPPOSZ                                               23
C -                                                                       23
      REAL TOFF_BPIPRMIN                                                  23
      EQUIVALENCE (BANK_TOFF(95),TOFF_BPIPRMIN,RE_TOFF_BPIPRMIN),         23
     *(NAMES_TOFF(43),NTOFF_BPIPRMIN)                                     23
      EQUIVALENCE (COMMEN_TOFF(43),CTOFF_BPIPRMIN),(MAP_TOFF(1,43),       23
     *MTOFF_BPIPRMIN)                                                     23
      CHARACTER CTOFF_BPIPRMIN*40/' '/                                    23
      CHARACTER NTOFF_BPIPRMIN*12/'R.BPIPRMIN'/                           23
      INTEGER MTOFF_BPIPRMIN(2)/1,1/                                      23
      REAL RE_TOFF_BPIPRMIN                                               23
C -                                                                       23
      REAL TOFF_BPIPRMAX                                                  23
      EQUIVALENCE (BANK_TOFF(96),TOFF_BPIPRMAX,RE_TOFF_BPIPRMAX),         23
     *(NAMES_TOFF(44),NTOFF_BPIPRMAX)                                     23
      EQUIVALENCE (COMMEN_TOFF(44),CTOFF_BPIPRMAX),(MAP_TOFF(1,44),       23
     *MTOFF_BPIPRMAX)                                                     23
      CHARACTER CTOFF_BPIPRMAX*40/' '/                                    23
      CHARACTER NTOFF_BPIPRMAX*12/'R.BPIPRMAX'/                           23
      INTEGER MTOFF_BPIPRMAX(2)/1,1/                                      23
      REAL RE_TOFF_BPIPRMAX                                               23
C -                                                                       23
      REAL TOFF_BPIPLENZ                                                  23
      EQUIVALENCE (BANK_TOFF(97),TOFF_BPIPLENZ,RE_TOFF_BPIPLENZ),         23
     *(NAMES_TOFF(45),NTOFF_BPIPLENZ)                                     23
      EQUIVALENCE (COMMEN_TOFF(45),CTOFF_BPIPLENZ),(MAP_TOFF(1,45),       23
     *MTOFF_BPIPLENZ)                                                     23
      CHARACTER CTOFF_BPIPLENZ*40/' '/                                    23
      CHARACTER NTOFF_BPIPLENZ*12/'R.BPIPLENZ'/                           23
      INTEGER MTOFF_BPIPLENZ(2)/1,1/                                      23
      REAL RE_TOFF_BPIPLENZ                                               23
C -                                                                       23
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                23
      PARAMETER (LEN_TOFF=97)                                             23
      PARAMETER (LLEN_TOFF=45)                                            23
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           23
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        23
      DATA IDNUM_TOFF/10*0/                                               23
      REAL BANK_TOFF(LEN_TOFF)                                            23
      CHARACTER*60 BPATH_TOFF/' '/                                        23
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          23
     *BPATH_TOFF                                                          23
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             23
C *                                                                       23
C -                                                                       27
      REAL MODR_HEIGHT                                                    27
      EQUIVALENCE (BANK_MODR(3),MODR_HEIGHT,RE_MODR_HEIGHT),              27
     *(NAMES_MODR(1),NMODR_HEIGHT)                                        27
      EQUIVALENCE (COMMEN_MODR(1),CMODR_HEIGHT),(MAP_MODR(1,1),           27
     *MMODR_HEIGHT)                                                       27
      CHARACTER CMODR_HEIGHT*40/' '/                                      27
      CHARACTER NMODR_HEIGHT*12/'R.HEIGHT'/                               27
      INTEGER MMODR_HEIGHT(2)/1,1/                                        27
      REAL RE_MODR_HEIGHT                                                 27
C -                                                                       27
      REAL MODR_WIDTH                                                     27
      EQUIVALENCE (BANK_MODR(4),MODR_WIDTH,RE_MODR_WIDTH),                27
     *(NAMES_MODR(2),NMODR_WIDTH)                                         27
      EQUIVALENCE (COMMEN_MODR(2),CMODR_WIDTH),(MAP_MODR(1,2),            27
     *MMODR_WIDTH)                                                        27
      CHARACTER CMODR_WIDTH*40/' '/                                       27
      CHARACTER NMODR_WIDTH*12/'R.WIDTH'/                                 27
      INTEGER MMODR_WIDTH(2)/1,1/                                         27
      REAL RE_MODR_WIDTH                                                  27
C -                                                                       27
      REAL MODR_LENGTH                                                    27
      EQUIVALENCE (BANK_MODR(5),MODR_LENGTH,RE_MODR_LENGTH),              27
     *(NAMES_MODR(3),NMODR_LENGTH)                                        27
      EQUIVALENCE (COMMEN_MODR(3),CMODR_LENGTH),(MAP_MODR(1,3),           27
     *MMODR_LENGTH)                                                       27
      CHARACTER CMODR_LENGTH*40/' '/                                      27
      CHARACTER NMODR_LENGTH*12/'R.LENGTH'/                               27
      INTEGER MMODR_LENGTH(2)/1,1/                                        27
      REAL RE_MODR_LENGTH                                                 27
C -                                                                       27
      REAL MODR_CENTER                                                    27
      EQUIVALENCE (BANK_MODR(6),MODR_CENTER,RE_MODR_CENTER),              27
     *(NAMES_MODR(4),NMODR_CENTER)                                        27
      EQUIVALENCE (COMMEN_MODR(4),CMODR_CENTER),(MAP_MODR(1,4),           27
     *MMODR_CENTER)                                                       27
      CHARACTER CMODR_CENTER*40/' '/                                      27
      CHARACTER NMODR_CENTER*12/'R.CENTER'/                               27
      INTEGER MMODR_CENTER(2)/1,1/                                        27
      REAL RE_MODR_CENTER                                                 27
C -                                                                       27
      REAL MODR_MRPCX(33)                                                 27
      EQUIVALENCE (BANK_MODR(7),MODR_MRPCX,RE_MODR_MRPCX),                27
     *(NAMES_MODR(5),NMODR_MRPCX)                                         27
      EQUIVALENCE (COMMEN_MODR(5),CMODR_MRPCX),(MAP_MODR(1,5),            27
     *MMODR_MRPCX)                                                        27
      CHARACTER CMODR_MRPCX*40/' '/                                       27
      CHARACTER NMODR_MRPCX*12/'R.MRPCX'/                                 27
      INTEGER MMODR_MRPCX(2)/33,1/                                        27
      REAL RE_MODR_MRPCX                                                  27
C -                                                                       27
      REAL MODR_MRPCZ(33)                                                 27
      EQUIVALENCE (BANK_MODR(40),MODR_MRPCZ,RE_MODR_MRPCZ),               27
     *(NAMES_MODR(6),NMODR_MRPCZ)                                         27
      EQUIVALENCE (COMMEN_MODR(6),CMODR_MRPCZ),(MAP_MODR(1,6),            27
     *MMODR_MRPCZ)                                                        27
      CHARACTER CMODR_MRPCZ*40/' '/                                       27
      CHARACTER NMODR_MRPCZ*12/'R.MRPCZ'/                                 27
      INTEGER MMODR_MRPCZ(2)/33,1/                                        27
      REAL RE_MODR_MRPCZ                                                  27
C -                                                                       27
      REAL MODR_MRPCA(33)                                                 27
      EQUIVALENCE (BANK_MODR(73),MODR_MRPCA,RE_MODR_MRPCA),               27
     *(NAMES_MODR(7),NMODR_MRPCA)                                         27
      EQUIVALENCE (COMMEN_MODR(7),CMODR_MRPCA),(MAP_MODR(1,7),            27
     *MMODR_MRPCA)                                                        27
      CHARACTER CMODR_MRPCA*40/' '/                                       27
      CHARACTER NMODR_MRPCA*12/'R.MRPCA'/                                 27
      INTEGER MMODR_MRPCA(2)/33,1/                                        27
      REAL RE_MODR_MRPCA                                                  27
C -                                                                       27
      REAL MODR_X0OFFSET                                                  27
      EQUIVALENCE (BANK_MODR(106),MODR_X0OFFSET,RE_MODR_X0OFFSET),        27
     *(NAMES_MODR(8),NMODR_X0OFFSET)                                      27
      EQUIVALENCE (COMMEN_MODR(8),CMODR_X0OFFSET),(MAP_MODR(1,8),         27
     *MMODR_X0OFFSET)                                                     27
      CHARACTER CMODR_X0OFFSET*40/' '/                                    27
      CHARACTER NMODR_X0OFFSET*12/'R.X0OFFSET'/                           27
      INTEGER MMODR_X0OFFSET(2)/1,1/                                      27
      REAL RE_MODR_X0OFFSET                                               27
C -                                                                       27
      REAL MODR_HCHGT                                                     27
      EQUIVALENCE (BANK_MODR(107),MODR_HCHGT,RE_MODR_HCHGT),              27
     *(NAMES_MODR(9),NMODR_HCHGT)                                         27
      EQUIVALENCE (COMMEN_MODR(9),CMODR_HCHGT),(MAP_MODR(1,9),            27
     *MMODR_HCHGT)                                                        27
      CHARACTER CMODR_HCHGT*40/' '/                                       27
      CHARACTER NMODR_HCHGT*12/'R.HCHGT'/                                 27
      INTEGER MMODR_HCHGT(2)/1,1/                                         27
      REAL RE_MODR_HCHGT                                                  27
C -                                                                       27
      REAL MODR_HCWID                                                     27
      EQUIVALENCE (BANK_MODR(108),MODR_HCWID,RE_MODR_HCWID),              27
     *(NAMES_MODR(10),NMODR_HCWID)                                        27
      EQUIVALENCE (COMMEN_MODR(10),CMODR_HCWID),(MAP_MODR(1,10),          27
     *MMODR_HCWID)                                                        27
      CHARACTER CMODR_HCWID*40/' '/                                       27
      CHARACTER NMODR_HCWID*12/'R.HCWID'/                                 27
      INTEGER MMODR_HCWID(2)/1,1/                                         27
      REAL RE_MODR_HCWID                                                  27
C -                                                                       27
      REAL MODR_HCLEN                                                     27
      EQUIVALENCE (BANK_MODR(109),MODR_HCLEN,RE_MODR_HCLEN),              27
     *(NAMES_MODR(11),NMODR_HCLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(11),CMODR_HCLEN),(MAP_MODR(1,11),          27
     *MMODR_HCLEN)                                                        27
      CHARACTER CMODR_HCLEN*40/' '/                                       27
      CHARACTER NMODR_HCLEN*12/'R.HCLEN'/                                 27
      INTEGER MMODR_HCLEN(2)/1,1/                                         27
      REAL RE_MODR_HCLEN                                                  27
C -                                                                       27
      REAL MODR_PCBHGT                                                    27
      EQUIVALENCE (BANK_MODR(110),MODR_PCBHGT,RE_MODR_PCBHGT),            27
     *(NAMES_MODR(12),NMODR_PCBHGT)                                       27
      EQUIVALENCE (COMMEN_MODR(12),CMODR_PCBHGT),(MAP_MODR(1,12),         27
     *MMODR_PCBHGT)                                                       27
      CHARACTER CMODR_PCBHGT*40/' '/                                      27
      CHARACTER NMODR_PCBHGT*12/'R.PCBHGT'/                               27
      INTEGER MMODR_PCBHGT(2)/1,1/                                        27
      REAL RE_MODR_PCBHGT                                                 27
C -                                                                       27
      REAL MODR_PCBWID                                                    27
      EQUIVALENCE (BANK_MODR(111),MODR_PCBWID,RE_MODR_PCBWID),            27
     *(NAMES_MODR(13),NMODR_PCBWID)                                       27
      EQUIVALENCE (COMMEN_MODR(13),CMODR_PCBWID),(MAP_MODR(1,13),         27
     *MMODR_PCBWID)                                                       27
      CHARACTER CMODR_PCBWID*40/' '/                                      27
      CHARACTER NMODR_PCBWID*12/'R.PCBWID'/                               27
      INTEGER MMODR_PCBWID(2)/1,1/                                        27
      REAL RE_MODR_PCBWID                                                 27
C -                                                                       27
      REAL MODR_PCBLEN                                                    27
      EQUIVALENCE (BANK_MODR(112),MODR_PCBLEN,RE_MODR_PCBLEN),            27
     *(NAMES_MODR(14),NMODR_PCBLEN)                                       27
      EQUIVALENCE (COMMEN_MODR(14),CMODR_PCBLEN),(MAP_MODR(1,14),         27
     *MMODR_PCBLEN)                                                       27
      CHARACTER CMODR_PCBLEN*40/' '/                                      27
      CHARACTER NMODR_PCBLEN*12/'R.PCBLEN'/                               27
      INTEGER MMODR_PCBLEN(2)/1,1/                                        27
      REAL RE_MODR_PCBLEN                                                 27
C -                                                                       27
      REAL MODR_MYHGT                                                     27
      EQUIVALENCE (BANK_MODR(113),MODR_MYHGT,RE_MODR_MYHGT),              27
     *(NAMES_MODR(15),NMODR_MYHGT)                                        27
      EQUIVALENCE (COMMEN_MODR(15),CMODR_MYHGT),(MAP_MODR(1,15),          27
     *MMODR_MYHGT)                                                        27
      CHARACTER CMODR_MYHGT*40/' '/                                       27
      CHARACTER NMODR_MYHGT*12/'R.MYHGT'/                                 27
      INTEGER MMODR_MYHGT(2)/1,1/                                         27
      REAL RE_MODR_MYHGT                                                  27
C -                                                                       27
      REAL MODR_MYWID                                                     27
      EQUIVALENCE (BANK_MODR(114),MODR_MYWID,RE_MODR_MYWID),              27
     *(NAMES_MODR(16),NMODR_MYWID)                                        27
      EQUIVALENCE (COMMEN_MODR(16),CMODR_MYWID),(MAP_MODR(1,16),          27
     *MMODR_MYWID)                                                        27
      CHARACTER CMODR_MYWID*40/' '/                                       27
      CHARACTER NMODR_MYWID*12/'R.MYWID'/                                 27
      INTEGER MMODR_MYWID(2)/1,1/                                         27
      REAL RE_MODR_MYWID                                                  27
C -                                                                       27
      REAL MODR_MYLEN                                                     27
      EQUIVALENCE (BANK_MODR(115),MODR_MYLEN,RE_MODR_MYLEN),              27
     *(NAMES_MODR(17),NMODR_MYLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(17),CMODR_MYLEN),(MAP_MODR(1,17),          27
     *MMODR_MYLEN)                                                        27
      CHARACTER CMODR_MYLEN*40/' '/                                       27
      CHARACTER NMODR_MYLEN*12/'R.MYLEN'/                                 27
      INTEGER MMODR_MYLEN(2)/1,1/                                         27
      REAL RE_MODR_MYLEN                                                  27
C -                                                                       27
      REAL MODR_GRHGT                                                     27
      EQUIVALENCE (BANK_MODR(116),MODR_GRHGT,RE_MODR_GRHGT),              27
     *(NAMES_MODR(18),NMODR_GRHGT)                                        27
      EQUIVALENCE (COMMEN_MODR(18),CMODR_GRHGT),(MAP_MODR(1,18),          27
     *MMODR_GRHGT)                                                        27
      CHARACTER CMODR_GRHGT*40/' '/                                       27
      CHARACTER NMODR_GRHGT*12/'R.GRHGT'/                                 27
      INTEGER MMODR_GRHGT(2)/1,1/                                         27
      REAL RE_MODR_GRHGT                                                  27
C -                                                                       27
      REAL MODR_GRWID                                                     27
      EQUIVALENCE (BANK_MODR(117),MODR_GRWID,RE_MODR_GRWID),              27
     *(NAMES_MODR(19),NMODR_GRWID)                                        27
      EQUIVALENCE (COMMEN_MODR(19),CMODR_GRWID),(MAP_MODR(1,19),          27
     *MMODR_GRWID)                                                        27
      CHARACTER CMODR_GRWID*40/' '/                                       27
      CHARACTER NMODR_GRWID*12/'R.GRWID'/                                 27
      INTEGER MMODR_GRWID(2)/1,1/                                         27
      REAL RE_MODR_GRWID                                                  27
C -                                                                       27
      REAL MODR_GRLEN                                                     27
      EQUIVALENCE (BANK_MODR(118),MODR_GRLEN,RE_MODR_GRLEN),              27
     *(NAMES_MODR(20),NMODR_GRLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(20),CMODR_GRLEN),(MAP_MODR(1,20),          27
     *MMODR_GRLEN)                                                        27
      CHARACTER CMODR_GRLEN*40/' '/                                       27
      CHARACTER NMODR_GRLEN*12/'R.GRLEN'/                                 27
      INTEGER MMODR_GRLEN(2)/1,1/                                         27
      REAL RE_MODR_GRLEN                                                  27
C -                                                                       27
      REAL MODR_OGHGT                                                     27
      EQUIVALENCE (BANK_MODR(119),MODR_OGHGT,RE_MODR_OGHGT),              27
     *(NAMES_MODR(21),NMODR_OGHGT)                                        27
      EQUIVALENCE (COMMEN_MODR(21),CMODR_OGHGT),(MAP_MODR(1,21),          27
     *MMODR_OGHGT)                                                        27
      CHARACTER CMODR_OGHGT*40/' '/                                       27
      CHARACTER NMODR_OGHGT*12/'R.OGHGT'/                                 27
      INTEGER MMODR_OGHGT(2)/1,1/                                         27
      REAL RE_MODR_OGHGT                                                  27
C -                                                                       27
      REAL MODR_OGWID                                                     27
      EQUIVALENCE (BANK_MODR(120),MODR_OGWID,RE_MODR_OGWID),              27
     *(NAMES_MODR(22),NMODR_OGWID)                                        27
      EQUIVALENCE (COMMEN_MODR(22),CMODR_OGWID),(MAP_MODR(1,22),          27
     *MMODR_OGWID)                                                        27
      CHARACTER CMODR_OGWID*40/' '/                                       27
      CHARACTER NMODR_OGWID*12/'R.OGWID'/                                 27
      INTEGER MMODR_OGWID(2)/1,1/                                         27
      REAL RE_MODR_OGWID                                                  27
C -                                                                       27
      REAL MODR_OGLEN                                                     27
      EQUIVALENCE (BANK_MODR(121),MODR_OGLEN,RE_MODR_OGLEN),              27
     *(NAMES_MODR(23),NMODR_OGLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(23),CMODR_OGLEN),(MAP_MODR(1,23),          27
     *MMODR_OGLEN)                                                        27
      CHARACTER CMODR_OGLEN*40/' '/                                       27
      CHARACTER NMODR_OGLEN*12/'R.OGLEN'/                                 27
      INTEGER MMODR_OGLEN(2)/1,1/                                         27
      REAL RE_MODR_OGLEN                                                  27
C -                                                                       27
      REAL MODR_IGHGT                                                     27
      EQUIVALENCE (BANK_MODR(122),MODR_IGHGT,RE_MODR_IGHGT),              27
     *(NAMES_MODR(24),NMODR_IGHGT)                                        27
      EQUIVALENCE (COMMEN_MODR(24),CMODR_IGHGT),(MAP_MODR(1,24),          27
     *MMODR_IGHGT)                                                        27
      CHARACTER CMODR_IGHGT*40/' '/                                       27
      CHARACTER NMODR_IGHGT*12/'R.IGHGT'/                                 27
      INTEGER MMODR_IGHGT(2)/1,1/                                         27
      REAL RE_MODR_IGHGT                                                  27
C -                                                                       27
      REAL MODR_IGWID                                                     27
      EQUIVALENCE (BANK_MODR(123),MODR_IGWID,RE_MODR_IGWID),              27
     *(NAMES_MODR(25),NMODR_IGWID)                                        27
      EQUIVALENCE (COMMEN_MODR(25),CMODR_IGWID),(MAP_MODR(1,25),          27
     *MMODR_IGWID)                                                        27
      CHARACTER CMODR_IGWID*40/' '/                                       27
      CHARACTER NMODR_IGWID*12/'R.IGWID'/                                 27
      INTEGER MMODR_IGWID(2)/1,1/                                         27
      REAL RE_MODR_IGWID                                                  27
C -                                                                       27
      REAL MODR_IGLEN                                                     27
      EQUIVALENCE (BANK_MODR(124),MODR_IGLEN,RE_MODR_IGLEN),              27
     *(NAMES_MODR(26),NMODR_IGLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(26),CMODR_IGLEN),(MAP_MODR(1,26),          27
     *MMODR_IGLEN)                                                        27
      CHARACTER CMODR_IGLEN*40/' '/                                       27
      CHARACTER NMODR_IGLEN*12/'R.IGLEN'/                                 27
      INTEGER MMODR_IGLEN(2)/1,1/                                         27
      REAL RE_MODR_IGLEN                                                  27
C -                                                                       27
      REAL MODR_SPRMIN                                                    27
      EQUIVALENCE (BANK_MODR(125),MODR_SPRMIN,RE_MODR_SPRMIN),            27
     *(NAMES_MODR(27),NMODR_SPRMIN)                                       27
      EQUIVALENCE (COMMEN_MODR(27),CMODR_SPRMIN),(MAP_MODR(1,27),         27
     *MMODR_SPRMIN)                                                       27
      CHARACTER CMODR_SPRMIN*40/' '/                                      27
      CHARACTER NMODR_SPRMIN*12/'R.SPRMIN'/                               27
      INTEGER MMODR_SPRMIN(2)/1,1/                                        27
      REAL RE_MODR_SPRMIN                                                 27
C -                                                                       27
      REAL MODR_SPRMAX                                                    27
      EQUIVALENCE (BANK_MODR(126),MODR_SPRMAX,RE_MODR_SPRMAX),            27
     *(NAMES_MODR(28),NMODR_SPRMAX)                                       27
      EQUIVALENCE (COMMEN_MODR(28),CMODR_SPRMAX),(MAP_MODR(1,28),         27
     *MMODR_SPRMAX)                                                       27
      CHARACTER CMODR_SPRMAX*40/' '/                                      27
      CHARACTER NMODR_SPRMAX*12/'R.SPRMAX'/                               27
      INTEGER MMODR_SPRMAX(2)/1,1/                                        27
      REAL RE_MODR_SPRMAX                                                 27
C -                                                                       27
      REAL MODR_SPLEN                                                     27
      EQUIVALENCE (BANK_MODR(127),MODR_SPLEN,RE_MODR_SPLEN),              27
     *(NAMES_MODR(29),NMODR_SPLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(29),CMODR_SPLEN),(MAP_MODR(1,29),          27
     *MMODR_SPLEN)                                                        27
      CHARACTER CMODR_SPLEN*40/' '/                                       27
      CHARACTER NMODR_SPLEN*12/'R.SPLEN'/                                 27
      INTEGER MMODR_SPLEN(2)/1,1/                                         27
      REAL RE_MODR_SPLEN                                                  27
C -                                                                       27
      REAL MODR_WGRMIN                                                    27
      EQUIVALENCE (BANK_MODR(128),MODR_WGRMIN,RE_MODR_WGRMIN),            27
     *(NAMES_MODR(30),NMODR_WGRMIN)                                       27
      EQUIVALENCE (COMMEN_MODR(30),CMODR_WGRMIN),(MAP_MODR(1,30),         27
     *MMODR_WGRMIN)                                                       27
      CHARACTER CMODR_WGRMIN*40/' '/                                      27
      CHARACTER NMODR_WGRMIN*12/'R.WGRMIN'/                               27
      INTEGER MMODR_WGRMIN(2)/1,1/                                        27
      REAL RE_MODR_WGRMIN                                                 27
C -                                                                       27
      REAL MODR_WGRMAX                                                    27
      EQUIVALENCE (BANK_MODR(129),MODR_WGRMAX,RE_MODR_WGRMAX),            27
     *(NAMES_MODR(31),NMODR_WGRMAX)                                       27
      EQUIVALENCE (COMMEN_MODR(31),CMODR_WGRMAX),(MAP_MODR(1,31),         27
     *MMODR_WGRMAX)                                                       27
      CHARACTER CMODR_WGRMAX*40/' '/                                      27
      CHARACTER NMODR_WGRMAX*12/'R.WGRMAX'/                               27
      INTEGER MMODR_WGRMAX(2)/1,1/                                        27
      REAL RE_MODR_WGRMAX                                                 27
C -                                                                       27
      REAL MODR_WGLEN                                                     27
      EQUIVALENCE (BANK_MODR(130),MODR_WGLEN,RE_MODR_WGLEN),              27
     *(NAMES_MODR(32),NMODR_WGLEN)                                        27
      EQUIVALENCE (COMMEN_MODR(32),CMODR_WGLEN),(MAP_MODR(1,32),          27
     *MMODR_WGLEN)                                                        27
      CHARACTER CMODR_WGLEN*40/' '/                                       27
      CHARACTER NMODR_WGLEN*12/'R.WGLEN'/                                 27
      INTEGER MMODR_WGLEN(2)/1,1/                                         27
      REAL RE_MODR_WGLEN                                                  27
C -                                                                       27
      REAL MODR_FEEH                                                      27
      EQUIVALENCE (BANK_MODR(131),MODR_FEEH,RE_MODR_FEEH),                27
     *(NAMES_MODR(33),NMODR_FEEH)                                         27
      EQUIVALENCE (COMMEN_MODR(33),CMODR_FEEH),(MAP_MODR(1,33),           27
     *MMODR_FEEH)                                                         27
      CHARACTER CMODR_FEEH*40/' '/                                        27
      CHARACTER NMODR_FEEH*12/'R.FEEH'/                                   27
      INTEGER MMODR_FEEH(2)/1,1/                                          27
      REAL RE_MODR_FEEH                                                   27
C -                                                                       27
      REAL MODR_HBWID                                                     27
      EQUIVALENCE (BANK_MODR(132),MODR_HBWID,RE_MODR_HBWID),              27
     *(NAMES_MODR(34),NMODR_HBWID)                                        27
      EQUIVALENCE (COMMEN_MODR(34),CMODR_HBWID),(MAP_MODR(1,34),          27
     *MMODR_HBWID)                                                        27
      CHARACTER CMODR_HBWID*40/' '/                                       27
      CHARACTER NMODR_HBWID*12/'R.HBWID'/                                 27
      INTEGER MMODR_HBWID(2)/1,1/                                         27
      REAL RE_MODR_HBWID                                                  27
C -                                                                       27
      REAL MODR_NGAP                                                      27
      EQUIVALENCE (BANK_MODR(133),MODR_NGAP,RE_MODR_NGAP),                27
     *(NAMES_MODR(35),NMODR_NGAP)                                         27
      EQUIVALENCE (COMMEN_MODR(35),CMODR_NGAP),(MAP_MODR(1,35),           27
     *MMODR_NGAP)                                                         27
      CHARACTER CMODR_NGAP*40/' '/                                        27
      CHARACTER NMODR_NGAP*12/'R.NGAP'/                                   27
      INTEGER MMODR_NGAP(2)/1,1/                                          27
      REAL RE_MODR_NGAP                                                   27
C -                                                                       27
      INTEGER LEN_MODR,LLEN_MODR,LINK_MODR                                27
      PARAMETER (LEN_MODR=133)                                            27
      PARAMETER (LLEN_MODR=35)                                            27
      INTEGER FLAG_MODR/0/,MAP_MODR(2,LLEN_MODR),IDNUM_MODR(10)           27
      CHARACTER NAMES_MODR(LLEN_MODR)*12,COMMEN_MODR(LLEN_MODR)*40        27
      DATA IDNUM_MODR/10*0/                                               27
      REAL BANK_MODR(LEN_MODR)                                            27
      CHARACTER*60 BPATH_MODR/' '/                                        27
      SAVE FLAG_MODR,MAP_MODR,IDNUM_MODR,NAMES_MODR,COMMEN_MODR,          27
     *BPATH_MODR                                                          27
      DATA LINK_MODR/0/,BANK_MODR/LEN_MODR*0/                             27
C *                                                                       27
C -                                                                       31
      REAL MOD4_HEIGHT                                                    31
      EQUIVALENCE (BANK_MOD4(3),MOD4_HEIGHT,RE_MOD4_HEIGHT),              31
     *(NAMES_MOD4(1),NMOD4_HEIGHT)                                        31
      EQUIVALENCE (COMMEN_MOD4(1),CMOD4_HEIGHT),(MAP_MOD4(1,1),           31
     *MMOD4_HEIGHT)                                                       31
      CHARACTER CMOD4_HEIGHT*40/' '/                                      31
      CHARACTER NMOD4_HEIGHT*12/'R.HEIGHT'/                               31
      INTEGER MMOD4_HEIGHT(2)/1,1/                                        31
      REAL RE_MOD4_HEIGHT                                                 31
C -                                                                       31
      REAL MOD4_WIDTH                                                     31
      EQUIVALENCE (BANK_MOD4(4),MOD4_WIDTH,RE_MOD4_WIDTH),                31
     *(NAMES_MOD4(2),NMOD4_WIDTH)                                         31
      EQUIVALENCE (COMMEN_MOD4(2),CMOD4_WIDTH),(MAP_MOD4(1,2),            31
     *MMOD4_WIDTH)                                                        31
      CHARACTER CMOD4_WIDTH*40/' '/                                       31
      CHARACTER NMOD4_WIDTH*12/'R.WIDTH'/                                 31
      INTEGER MMOD4_WIDTH(2)/1,1/                                         31
      REAL RE_MOD4_WIDTH                                                  31
C -                                                                       31
      REAL MOD4_LENGTH                                                    31
      EQUIVALENCE (BANK_MOD4(5),MOD4_LENGTH,RE_MOD4_LENGTH),              31
     *(NAMES_MOD4(3),NMOD4_LENGTH)                                        31
      EQUIVALENCE (COMMEN_MOD4(3),CMOD4_LENGTH),(MAP_MOD4(1,3),           31
     *MMOD4_LENGTH)                                                       31
      CHARACTER CMOD4_LENGTH*40/' '/                                      31
      CHARACTER NMOD4_LENGTH*12/'R.LENGTH'/                               31
      INTEGER MMOD4_LENGTH(2)/1,1/                                        31
      REAL RE_MOD4_LENGTH                                                 31
C -                                                                       31
      REAL MOD4_CENTER                                                    31
      EQUIVALENCE (BANK_MOD4(6),MOD4_CENTER,RE_MOD4_CENTER),              31
     *(NAMES_MOD4(4),NMOD4_CENTER)                                        31
      EQUIVALENCE (COMMEN_MOD4(4),CMOD4_CENTER),(MAP_MOD4(1,4),           31
     *MMOD4_CENTER)                                                       31
      CHARACTER CMOD4_CENTER*40/' '/                                      31
      CHARACTER NMOD4_CENTER*12/'R.CENTER'/                               31
      INTEGER MMOD4_CENTER(2)/1,1/                                        31
      REAL RE_MOD4_CENTER                                                 31
C -                                                                       31
      REAL MOD4_MRPCX(32)                                                 31
      EQUIVALENCE (BANK_MOD4(7),MOD4_MRPCX,RE_MOD4_MRPCX),                31
     *(NAMES_MOD4(5),NMOD4_MRPCX)                                         31
      EQUIVALENCE (COMMEN_MOD4(5),CMOD4_MRPCX),(MAP_MOD4(1,5),            31
     *MMOD4_MRPCX)                                                        31
      CHARACTER CMOD4_MRPCX*40/' '/                                       31
      CHARACTER NMOD4_MRPCX*12/'R.MRPCX'/                                 31
      INTEGER MMOD4_MRPCX(2)/32,1/                                        31
      REAL RE_MOD4_MRPCX                                                  31
C -                                                                       31
      REAL MOD4_MRPCZ(32)                                                 31
      EQUIVALENCE (BANK_MOD4(39),MOD4_MRPCZ,RE_MOD4_MRPCZ),               31
     *(NAMES_MOD4(6),NMOD4_MRPCZ)                                         31
      EQUIVALENCE (COMMEN_MOD4(6),CMOD4_MRPCZ),(MAP_MOD4(1,6),            31
     *MMOD4_MRPCZ)                                                        31
      CHARACTER CMOD4_MRPCZ*40/' '/                                       31
      CHARACTER NMOD4_MRPCZ*12/'R.MRPCZ'/                                 31
      INTEGER MMOD4_MRPCZ(2)/32,1/                                        31
      REAL RE_MOD4_MRPCZ                                                  31
C -                                                                       31
      REAL MOD4_MRPCA(32)                                                 31
      EQUIVALENCE (BANK_MOD4(71),MOD4_MRPCA,RE_MOD4_MRPCA),               31
     *(NAMES_MOD4(7),NMOD4_MRPCA)                                         31
      EQUIVALENCE (COMMEN_MOD4(7),CMOD4_MRPCA),(MAP_MOD4(1,7),            31
     *MMOD4_MRPCA)                                                        31
      CHARACTER CMOD4_MRPCA*40/' '/                                       31
      CHARACTER NMOD4_MRPCA*12/'R.MRPCA'/                                 31
      INTEGER MMOD4_MRPCA(2)/32,1/                                        31
      REAL RE_MOD4_MRPCA                                                  31
C -                                                                       31
      REAL MOD4_X0OFFSET                                                  31
      EQUIVALENCE (BANK_MOD4(103),MOD4_X0OFFSET,RE_MOD4_X0OFFSET),        31
     *(NAMES_MOD4(8),NMOD4_X0OFFSET)                                      31
      EQUIVALENCE (COMMEN_MOD4(8),CMOD4_X0OFFSET),(MAP_MOD4(1,8),         31
     *MMOD4_X0OFFSET)                                                     31
      CHARACTER CMOD4_X0OFFSET*40/' '/                                    31
      CHARACTER NMOD4_X0OFFSET*12/'R.X0OFFSET'/                           31
      INTEGER MMOD4_X0OFFSET(2)/1,1/                                      31
      REAL RE_MOD4_X0OFFSET                                               31
C -                                                                       31
      REAL MOD4_HCHGT                                                     31
      EQUIVALENCE (BANK_MOD4(104),MOD4_HCHGT,RE_MOD4_HCHGT),              31
     *(NAMES_MOD4(9),NMOD4_HCHGT)                                         31
      EQUIVALENCE (COMMEN_MOD4(9),CMOD4_HCHGT),(MAP_MOD4(1,9),            31
     *MMOD4_HCHGT)                                                        31
      CHARACTER CMOD4_HCHGT*40/' '/                                       31
      CHARACTER NMOD4_HCHGT*12/'R.HCHGT'/                                 31
      INTEGER MMOD4_HCHGT(2)/1,1/                                         31
      REAL RE_MOD4_HCHGT                                                  31
C -                                                                       31
      REAL MOD4_HCWID                                                     31
      EQUIVALENCE (BANK_MOD4(105),MOD4_HCWID,RE_MOD4_HCWID),              31
     *(NAMES_MOD4(10),NMOD4_HCWID)                                        31
      EQUIVALENCE (COMMEN_MOD4(10),CMOD4_HCWID),(MAP_MOD4(1,10),          31
     *MMOD4_HCWID)                                                        31
      CHARACTER CMOD4_HCWID*40/' '/                                       31
      CHARACTER NMOD4_HCWID*12/'R.HCWID'/                                 31
      INTEGER MMOD4_HCWID(2)/1,1/                                         31
      REAL RE_MOD4_HCWID                                                  31
C -                                                                       31
      REAL MOD4_HCLEN                                                     31
      EQUIVALENCE (BANK_MOD4(106),MOD4_HCLEN,RE_MOD4_HCLEN),              31
     *(NAMES_MOD4(11),NMOD4_HCLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(11),CMOD4_HCLEN),(MAP_MOD4(1,11),          31
     *MMOD4_HCLEN)                                                        31
      CHARACTER CMOD4_HCLEN*40/' '/                                       31
      CHARACTER NMOD4_HCLEN*12/'R.HCLEN'/                                 31
      INTEGER MMOD4_HCLEN(2)/1,1/                                         31
      REAL RE_MOD4_HCLEN                                                  31
C -                                                                       31
      REAL MOD4_PCBHGT                                                    31
      EQUIVALENCE (BANK_MOD4(107),MOD4_PCBHGT,RE_MOD4_PCBHGT),            31
     *(NAMES_MOD4(12),NMOD4_PCBHGT)                                       31
      EQUIVALENCE (COMMEN_MOD4(12),CMOD4_PCBHGT),(MAP_MOD4(1,12),         31
     *MMOD4_PCBHGT)                                                       31
      CHARACTER CMOD4_PCBHGT*40/' '/                                      31
      CHARACTER NMOD4_PCBHGT*12/'R.PCBHGT'/                               31
      INTEGER MMOD4_PCBHGT(2)/1,1/                                        31
      REAL RE_MOD4_PCBHGT                                                 31
C -                                                                       31
      REAL MOD4_PCBWID                                                    31
      EQUIVALENCE (BANK_MOD4(108),MOD4_PCBWID,RE_MOD4_PCBWID),            31
     *(NAMES_MOD4(13),NMOD4_PCBWID)                                       31
      EQUIVALENCE (COMMEN_MOD4(13),CMOD4_PCBWID),(MAP_MOD4(1,13),         31
     *MMOD4_PCBWID)                                                       31
      CHARACTER CMOD4_PCBWID*40/' '/                                      31
      CHARACTER NMOD4_PCBWID*12/'R.PCBWID'/                               31
      INTEGER MMOD4_PCBWID(2)/1,1/                                        31
      REAL RE_MOD4_PCBWID                                                 31
C -                                                                       31
      REAL MOD4_PCBLEN                                                    31
      EQUIVALENCE (BANK_MOD4(109),MOD4_PCBLEN,RE_MOD4_PCBLEN),            31
     *(NAMES_MOD4(14),NMOD4_PCBLEN)                                       31
      EQUIVALENCE (COMMEN_MOD4(14),CMOD4_PCBLEN),(MAP_MOD4(1,14),         31
     *MMOD4_PCBLEN)                                                       31
      CHARACTER CMOD4_PCBLEN*40/' '/                                      31
      CHARACTER NMOD4_PCBLEN*12/'R.PCBLEN'/                               31
      INTEGER MMOD4_PCBLEN(2)/1,1/                                        31
      REAL RE_MOD4_PCBLEN                                                 31
C -                                                                       31
      REAL MOD4_MYHGT                                                     31
      EQUIVALENCE (BANK_MOD4(110),MOD4_MYHGT,RE_MOD4_MYHGT),              31
     *(NAMES_MOD4(15),NMOD4_MYHGT)                                        31
      EQUIVALENCE (COMMEN_MOD4(15),CMOD4_MYHGT),(MAP_MOD4(1,15),          31
     *MMOD4_MYHGT)                                                        31
      CHARACTER CMOD4_MYHGT*40/' '/                                       31
      CHARACTER NMOD4_MYHGT*12/'R.MYHGT'/                                 31
      INTEGER MMOD4_MYHGT(2)/1,1/                                         31
      REAL RE_MOD4_MYHGT                                                  31
C -                                                                       31
      REAL MOD4_MYWID                                                     31
      EQUIVALENCE (BANK_MOD4(111),MOD4_MYWID,RE_MOD4_MYWID),              31
     *(NAMES_MOD4(16),NMOD4_MYWID)                                        31
      EQUIVALENCE (COMMEN_MOD4(16),CMOD4_MYWID),(MAP_MOD4(1,16),          31
     *MMOD4_MYWID)                                                        31
      CHARACTER CMOD4_MYWID*40/' '/                                       31
      CHARACTER NMOD4_MYWID*12/'R.MYWID'/                                 31
      INTEGER MMOD4_MYWID(2)/1,1/                                         31
      REAL RE_MOD4_MYWID                                                  31
C -                                                                       31
      REAL MOD4_MYLEN                                                     31
      EQUIVALENCE (BANK_MOD4(112),MOD4_MYLEN,RE_MOD4_MYLEN),              31
     *(NAMES_MOD4(17),NMOD4_MYLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(17),CMOD4_MYLEN),(MAP_MOD4(1,17),          31
     *MMOD4_MYLEN)                                                        31
      CHARACTER CMOD4_MYLEN*40/' '/                                       31
      CHARACTER NMOD4_MYLEN*12/'R.MYLEN'/                                 31
      INTEGER MMOD4_MYLEN(2)/1,1/                                         31
      REAL RE_MOD4_MYLEN                                                  31
C -                                                                       31
      REAL MOD4_GRHGT                                                     31
      EQUIVALENCE (BANK_MOD4(113),MOD4_GRHGT,RE_MOD4_GRHGT),              31
     *(NAMES_MOD4(18),NMOD4_GRHGT)                                        31
      EQUIVALENCE (COMMEN_MOD4(18),CMOD4_GRHGT),(MAP_MOD4(1,18),          31
     *MMOD4_GRHGT)                                                        31
      CHARACTER CMOD4_GRHGT*40/' '/                                       31
      CHARACTER NMOD4_GRHGT*12/'R.GRHGT'/                                 31
      INTEGER MMOD4_GRHGT(2)/1,1/                                         31
      REAL RE_MOD4_GRHGT                                                  31
C -                                                                       31
      REAL MOD4_GRWID                                                     31
      EQUIVALENCE (BANK_MOD4(114),MOD4_GRWID,RE_MOD4_GRWID),              31
     *(NAMES_MOD4(19),NMOD4_GRWID)                                        31
      EQUIVALENCE (COMMEN_MOD4(19),CMOD4_GRWID),(MAP_MOD4(1,19),          31
     *MMOD4_GRWID)                                                        31
      CHARACTER CMOD4_GRWID*40/' '/                                       31
      CHARACTER NMOD4_GRWID*12/'R.GRWID'/                                 31
      INTEGER MMOD4_GRWID(2)/1,1/                                         31
      REAL RE_MOD4_GRWID                                                  31
C -                                                                       31
      REAL MOD4_GRLEN                                                     31
      EQUIVALENCE (BANK_MOD4(115),MOD4_GRLEN,RE_MOD4_GRLEN),              31
     *(NAMES_MOD4(20),NMOD4_GRLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(20),CMOD4_GRLEN),(MAP_MOD4(1,20),          31
     *MMOD4_GRLEN)                                                        31
      CHARACTER CMOD4_GRLEN*40/' '/                                       31
      CHARACTER NMOD4_GRLEN*12/'R.GRLEN'/                                 31
      INTEGER MMOD4_GRLEN(2)/1,1/                                         31
      REAL RE_MOD4_GRLEN                                                  31
C -                                                                       31
      REAL MOD4_OGHGT                                                     31
      EQUIVALENCE (BANK_MOD4(116),MOD4_OGHGT,RE_MOD4_OGHGT),              31
     *(NAMES_MOD4(21),NMOD4_OGHGT)                                        31
      EQUIVALENCE (COMMEN_MOD4(21),CMOD4_OGHGT),(MAP_MOD4(1,21),          31
     *MMOD4_OGHGT)                                                        31
      CHARACTER CMOD4_OGHGT*40/' '/                                       31
      CHARACTER NMOD4_OGHGT*12/'R.OGHGT'/                                 31
      INTEGER MMOD4_OGHGT(2)/1,1/                                         31
      REAL RE_MOD4_OGHGT                                                  31
C -                                                                       31
      REAL MOD4_OGWID                                                     31
      EQUIVALENCE (BANK_MOD4(117),MOD4_OGWID,RE_MOD4_OGWID),              31
     *(NAMES_MOD4(22),NMOD4_OGWID)                                        31
      EQUIVALENCE (COMMEN_MOD4(22),CMOD4_OGWID),(MAP_MOD4(1,22),          31
     *MMOD4_OGWID)                                                        31
      CHARACTER CMOD4_OGWID*40/' '/                                       31
      CHARACTER NMOD4_OGWID*12/'R.OGWID'/                                 31
      INTEGER MMOD4_OGWID(2)/1,1/                                         31
      REAL RE_MOD4_OGWID                                                  31
C -                                                                       31
      REAL MOD4_OGLEN                                                     31
      EQUIVALENCE (BANK_MOD4(118),MOD4_OGLEN,RE_MOD4_OGLEN),              31
     *(NAMES_MOD4(23),NMOD4_OGLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(23),CMOD4_OGLEN),(MAP_MOD4(1,23),          31
     *MMOD4_OGLEN)                                                        31
      CHARACTER CMOD4_OGLEN*40/' '/                                       31
      CHARACTER NMOD4_OGLEN*12/'R.OGLEN'/                                 31
      INTEGER MMOD4_OGLEN(2)/1,1/                                         31
      REAL RE_MOD4_OGLEN                                                  31
C -                                                                       31
      REAL MOD4_IGHGT                                                     31
      EQUIVALENCE (BANK_MOD4(119),MOD4_IGHGT,RE_MOD4_IGHGT),              31
     *(NAMES_MOD4(24),NMOD4_IGHGT)                                        31
      EQUIVALENCE (COMMEN_MOD4(24),CMOD4_IGHGT),(MAP_MOD4(1,24),          31
     *MMOD4_IGHGT)                                                        31
      CHARACTER CMOD4_IGHGT*40/' '/                                       31
      CHARACTER NMOD4_IGHGT*12/'R.IGHGT'/                                 31
      INTEGER MMOD4_IGHGT(2)/1,1/                                         31
      REAL RE_MOD4_IGHGT                                                  31
C -                                                                       31
      REAL MOD4_IGWID                                                     31
      EQUIVALENCE (BANK_MOD4(120),MOD4_IGWID,RE_MOD4_IGWID),              31
     *(NAMES_MOD4(25),NMOD4_IGWID)                                        31
      EQUIVALENCE (COMMEN_MOD4(25),CMOD4_IGWID),(MAP_MOD4(1,25),          31
     *MMOD4_IGWID)                                                        31
      CHARACTER CMOD4_IGWID*40/' '/                                       31
      CHARACTER NMOD4_IGWID*12/'R.IGWID'/                                 31
      INTEGER MMOD4_IGWID(2)/1,1/                                         31
      REAL RE_MOD4_IGWID                                                  31
C -                                                                       31
      REAL MOD4_IGLEN                                                     31
      EQUIVALENCE (BANK_MOD4(121),MOD4_IGLEN,RE_MOD4_IGLEN),              31
     *(NAMES_MOD4(26),NMOD4_IGLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(26),CMOD4_IGLEN),(MAP_MOD4(1,26),          31
     *MMOD4_IGLEN)                                                        31
      CHARACTER CMOD4_IGLEN*40/' '/                                       31
      CHARACTER NMOD4_IGLEN*12/'R.IGLEN'/                                 31
      INTEGER MMOD4_IGLEN(2)/1,1/                                         31
      REAL RE_MOD4_IGLEN                                                  31
C -                                                                       31
      REAL MOD4_SPRMIN                                                    31
      EQUIVALENCE (BANK_MOD4(122),MOD4_SPRMIN,RE_MOD4_SPRMIN),            31
     *(NAMES_MOD4(27),NMOD4_SPRMIN)                                       31
      EQUIVALENCE (COMMEN_MOD4(27),CMOD4_SPRMIN),(MAP_MOD4(1,27),         31
     *MMOD4_SPRMIN)                                                       31
      CHARACTER CMOD4_SPRMIN*40/' '/                                      31
      CHARACTER NMOD4_SPRMIN*12/'R.SPRMIN'/                               31
      INTEGER MMOD4_SPRMIN(2)/1,1/                                        31
      REAL RE_MOD4_SPRMIN                                                 31
C -                                                                       31
      REAL MOD4_SPRMAX                                                    31
      EQUIVALENCE (BANK_MOD4(123),MOD4_SPRMAX,RE_MOD4_SPRMAX),            31
     *(NAMES_MOD4(28),NMOD4_SPRMAX)                                       31
      EQUIVALENCE (COMMEN_MOD4(28),CMOD4_SPRMAX),(MAP_MOD4(1,28),         31
     *MMOD4_SPRMAX)                                                       31
      CHARACTER CMOD4_SPRMAX*40/' '/                                      31
      CHARACTER NMOD4_SPRMAX*12/'R.SPRMAX'/                               31
      INTEGER MMOD4_SPRMAX(2)/1,1/                                        31
      REAL RE_MOD4_SPRMAX                                                 31
C -                                                                       31
      REAL MOD4_SPLEN                                                     31
      EQUIVALENCE (BANK_MOD4(124),MOD4_SPLEN,RE_MOD4_SPLEN),              31
     *(NAMES_MOD4(29),NMOD4_SPLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(29),CMOD4_SPLEN),(MAP_MOD4(1,29),          31
     *MMOD4_SPLEN)                                                        31
      CHARACTER CMOD4_SPLEN*40/' '/                                       31
      CHARACTER NMOD4_SPLEN*12/'R.SPLEN'/                                 31
      INTEGER MMOD4_SPLEN(2)/1,1/                                         31
      REAL RE_MOD4_SPLEN                                                  31
C -                                                                       31
      REAL MOD4_WGRMIN                                                    31
      EQUIVALENCE (BANK_MOD4(125),MOD4_WGRMIN,RE_MOD4_WGRMIN),            31
     *(NAMES_MOD4(30),NMOD4_WGRMIN)                                       31
      EQUIVALENCE (COMMEN_MOD4(30),CMOD4_WGRMIN),(MAP_MOD4(1,30),         31
     *MMOD4_WGRMIN)                                                       31
      CHARACTER CMOD4_WGRMIN*40/' '/                                      31
      CHARACTER NMOD4_WGRMIN*12/'R.WGRMIN'/                               31
      INTEGER MMOD4_WGRMIN(2)/1,1/                                        31
      REAL RE_MOD4_WGRMIN                                                 31
C -                                                                       31
      REAL MOD4_WGRMAX                                                    31
      EQUIVALENCE (BANK_MOD4(126),MOD4_WGRMAX,RE_MOD4_WGRMAX),            31
     *(NAMES_MOD4(31),NMOD4_WGRMAX)                                       31
      EQUIVALENCE (COMMEN_MOD4(31),CMOD4_WGRMAX),(MAP_MOD4(1,31),         31
     *MMOD4_WGRMAX)                                                       31
      CHARACTER CMOD4_WGRMAX*40/' '/                                      31
      CHARACTER NMOD4_WGRMAX*12/'R.WGRMAX'/                               31
      INTEGER MMOD4_WGRMAX(2)/1,1/                                        31
      REAL RE_MOD4_WGRMAX                                                 31
C -                                                                       31
      REAL MOD4_WGLEN                                                     31
      EQUIVALENCE (BANK_MOD4(127),MOD4_WGLEN,RE_MOD4_WGLEN),              31
     *(NAMES_MOD4(32),NMOD4_WGLEN)                                        31
      EQUIVALENCE (COMMEN_MOD4(32),CMOD4_WGLEN),(MAP_MOD4(1,32),          31
     *MMOD4_WGLEN)                                                        31
      CHARACTER CMOD4_WGLEN*40/' '/                                       31
      CHARACTER NMOD4_WGLEN*12/'R.WGLEN'/                                 31
      INTEGER MMOD4_WGLEN(2)/1,1/                                         31
      REAL RE_MOD4_WGLEN                                                  31
C -                                                                       31
      REAL MOD4_FEEH                                                      31
      EQUIVALENCE (BANK_MOD4(128),MOD4_FEEH,RE_MOD4_FEEH),                31
     *(NAMES_MOD4(33),NMOD4_FEEH)                                         31
      EQUIVALENCE (COMMEN_MOD4(33),CMOD4_FEEH),(MAP_MOD4(1,33),           31
     *MMOD4_FEEH)                                                         31
      CHARACTER CMOD4_FEEH*40/' '/                                        31
      CHARACTER NMOD4_FEEH*12/'R.FEEH'/                                   31
      INTEGER MMOD4_FEEH(2)/1,1/                                          31
      REAL RE_MOD4_FEEH                                                   31
C -                                                                       31
      REAL MOD4_HBWID                                                     31
      EQUIVALENCE (BANK_MOD4(129),MOD4_HBWID,RE_MOD4_HBWID),              31
     *(NAMES_MOD4(34),NMOD4_HBWID)                                        31
      EQUIVALENCE (COMMEN_MOD4(34),CMOD4_HBWID),(MAP_MOD4(1,34),          31
     *MMOD4_HBWID)                                                        31
      CHARACTER CMOD4_HBWID*40/' '/                                       31
      CHARACTER NMOD4_HBWID*12/'R.HBWID'/                                 31
      INTEGER MMOD4_HBWID(2)/1,1/                                         31
      REAL RE_MOD4_HBWID                                                  31
C -                                                                       31
      REAL MOD4_NGAP                                                      31
      EQUIVALENCE (BANK_MOD4(130),MOD4_NGAP,RE_MOD4_NGAP),                31
     *(NAMES_MOD4(35),NMOD4_NGAP)                                         31
      EQUIVALENCE (COMMEN_MOD4(35),CMOD4_NGAP),(MAP_MOD4(1,35),           31
     *MMOD4_NGAP)                                                         31
      CHARACTER CMOD4_NGAP*40/' '/                                        31
      CHARACTER NMOD4_NGAP*12/'R.NGAP'/                                   31
      INTEGER MMOD4_NGAP(2)/1,1/                                          31
      REAL RE_MOD4_NGAP                                                   31
C -                                                                       31
      REAL MOD4_TRAYEDGEZ                                                 31
      EQUIVALENCE (BANK_MOD4(131),MOD4_TRAYEDGEZ,RE_MOD4_TRAYEDGEZ),      31
     *(NAMES_MOD4(36),NMOD4_TRAYEDGEZ)                                    31
      EQUIVALENCE (COMMEN_MOD4(36),CMOD4_TRAYEDGEZ),(MAP_MOD4(1,36),      31
     *MMOD4_TRAYEDGEZ)                                                    31
      CHARACTER CMOD4_TRAYEDGEZ*40/' '/                                   31
      CHARACTER NMOD4_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         31
      INTEGER MMOD4_TRAYEDGEZ(2)/1,1/                                     31
      REAL RE_MOD4_TRAYEDGEZ                                              31
C -                                                                       31
      INTEGER LEN_MOD4,LLEN_MOD4,LINK_MOD4                                31
      PARAMETER (LEN_MOD4=131)                                            31
      PARAMETER (LLEN_MOD4=36)                                            31
      INTEGER FLAG_MOD4/0/,MAP_MOD4(2,LLEN_MOD4),IDNUM_MOD4(10)           31
      CHARACTER NAMES_MOD4(LLEN_MOD4)*12,COMMEN_MOD4(LLEN_MOD4)*40        31
      DATA IDNUM_MOD4/10*0/                                               31
      REAL BANK_MOD4(LEN_MOD4)                                            31
      CHARACTER*60 BPATH_MOD4/' '/                                        31
      SAVE FLAG_MOD4,MAP_MOD4,IDNUM_MOD4,NAMES_MOD4,COMMEN_MOD4,          31
     *BPATH_MOD4                                                          31
      DATA LINK_MOD4/0/,BANK_MOD4/LEN_MOD4*0/                             31
C *                                                                       31
C -                                                                       35
      REAL MOD5_HEIGHT                                                    35
      EQUIVALENCE (BANK_MOD5(3),MOD5_HEIGHT,RE_MOD5_HEIGHT),              35
     *(NAMES_MOD5(1),NMOD5_HEIGHT)                                        35
      EQUIVALENCE (COMMEN_MOD5(1),CMOD5_HEIGHT),(MAP_MOD5(1,1),           35
     *MMOD5_HEIGHT)                                                       35
      CHARACTER CMOD5_HEIGHT*40/' '/                                      35
      CHARACTER NMOD5_HEIGHT*12/'R.HEIGHT'/                               35
      INTEGER MMOD5_HEIGHT(2)/1,1/                                        35
      REAL RE_MOD5_HEIGHT                                                 35
C -                                                                       35
      REAL MOD5_WIDTH                                                     35
      EQUIVALENCE (BANK_MOD5(4),MOD5_WIDTH,RE_MOD5_WIDTH),                35
     *(NAMES_MOD5(2),NMOD5_WIDTH)                                         35
      EQUIVALENCE (COMMEN_MOD5(2),CMOD5_WIDTH),(MAP_MOD5(1,2),            35
     *MMOD5_WIDTH)                                                        35
      CHARACTER CMOD5_WIDTH*40/' '/                                       35
      CHARACTER NMOD5_WIDTH*12/'R.WIDTH'/                                 35
      INTEGER MMOD5_WIDTH(2)/1,1/                                         35
      REAL RE_MOD5_WIDTH                                                  35
C -                                                                       35
      REAL MOD5_LENGTH                                                    35
      EQUIVALENCE (BANK_MOD5(5),MOD5_LENGTH,RE_MOD5_LENGTH),              35
     *(NAMES_MOD5(3),NMOD5_LENGTH)                                        35
      EQUIVALENCE (COMMEN_MOD5(3),CMOD5_LENGTH),(MAP_MOD5(1,3),           35
     *MMOD5_LENGTH)                                                       35
      CHARACTER CMOD5_LENGTH*40/' '/                                      35
      CHARACTER NMOD5_LENGTH*12/'R.LENGTH'/                               35
      INTEGER MMOD5_LENGTH(2)/1,1/                                        35
      REAL RE_MOD5_LENGTH                                                 35
C -                                                                       35
      REAL MOD5_CENTER                                                    35
      EQUIVALENCE (BANK_MOD5(6),MOD5_CENTER,RE_MOD5_CENTER),              35
     *(NAMES_MOD5(4),NMOD5_CENTER)                                        35
      EQUIVALENCE (COMMEN_MOD5(4),CMOD5_CENTER),(MAP_MOD5(1,4),           35
     *MMOD5_CENTER)                                                       35
      CHARACTER CMOD5_CENTER*40/' '/                                      35
      CHARACTER NMOD5_CENTER*12/'R.CENTER'/                               35
      INTEGER MMOD5_CENTER(2)/1,1/                                        35
      REAL RE_MOD5_CENTER                                                 35
C -                                                                       35
      REAL MOD5_MRPCX(32)                                                 35
      EQUIVALENCE (BANK_MOD5(7),MOD5_MRPCX,RE_MOD5_MRPCX),                35
     *(NAMES_MOD5(5),NMOD5_MRPCX)                                         35
      EQUIVALENCE (COMMEN_MOD5(5),CMOD5_MRPCX),(MAP_MOD5(1,5),            35
     *MMOD5_MRPCX)                                                        35
      CHARACTER CMOD5_MRPCX*40/' '/                                       35
      CHARACTER NMOD5_MRPCX*12/'R.MRPCX'/                                 35
      INTEGER MMOD5_MRPCX(2)/32,1/                                        35
      REAL RE_MOD5_MRPCX                                                  35
C -                                                                       35
      REAL MOD5_MRPCZ(32)                                                 35
      EQUIVALENCE (BANK_MOD5(39),MOD5_MRPCZ,RE_MOD5_MRPCZ),               35
     *(NAMES_MOD5(6),NMOD5_MRPCZ)                                         35
      EQUIVALENCE (COMMEN_MOD5(6),CMOD5_MRPCZ),(MAP_MOD5(1,6),            35
     *MMOD5_MRPCZ)                                                        35
      CHARACTER CMOD5_MRPCZ*40/' '/                                       35
      CHARACTER NMOD5_MRPCZ*12/'R.MRPCZ'/                                 35
      INTEGER MMOD5_MRPCZ(2)/32,1/                                        35
      REAL RE_MOD5_MRPCZ                                                  35
C -                                                                       35
      REAL MOD5_MRPCA(32)                                                 35
      EQUIVALENCE (BANK_MOD5(71),MOD5_MRPCA,RE_MOD5_MRPCA),               35
     *(NAMES_MOD5(7),NMOD5_MRPCA)                                         35
      EQUIVALENCE (COMMEN_MOD5(7),CMOD5_MRPCA),(MAP_MOD5(1,7),            35
     *MMOD5_MRPCA)                                                        35
      CHARACTER CMOD5_MRPCA*40/' '/                                       35
      CHARACTER NMOD5_MRPCA*12/'R.MRPCA'/                                 35
      INTEGER MMOD5_MRPCA(2)/32,1/                                        35
      REAL RE_MOD5_MRPCA                                                  35
C -                                                                       35
      REAL MOD5_X0OFFSET                                                  35
      EQUIVALENCE (BANK_MOD5(103),MOD5_X0OFFSET,RE_MOD5_X0OFFSET),        35
     *(NAMES_MOD5(8),NMOD5_X0OFFSET)                                      35
      EQUIVALENCE (COMMEN_MOD5(8),CMOD5_X0OFFSET),(MAP_MOD5(1,8),         35
     *MMOD5_X0OFFSET)                                                     35
      CHARACTER CMOD5_X0OFFSET*40/' '/                                    35
      CHARACTER NMOD5_X0OFFSET*12/'R.X0OFFSET'/                           35
      INTEGER MMOD5_X0OFFSET(2)/1,1/                                      35
      REAL RE_MOD5_X0OFFSET                                               35
C -                                                                       35
      REAL MOD5_HCHGT                                                     35
      EQUIVALENCE (BANK_MOD5(104),MOD5_HCHGT,RE_MOD5_HCHGT),              35
     *(NAMES_MOD5(9),NMOD5_HCHGT)                                         35
      EQUIVALENCE (COMMEN_MOD5(9),CMOD5_HCHGT),(MAP_MOD5(1,9),            35
     *MMOD5_HCHGT)                                                        35
      CHARACTER CMOD5_HCHGT*40/' '/                                       35
      CHARACTER NMOD5_HCHGT*12/'R.HCHGT'/                                 35
      INTEGER MMOD5_HCHGT(2)/1,1/                                         35
      REAL RE_MOD5_HCHGT                                                  35
C -                                                                       35
      REAL MOD5_HCWID                                                     35
      EQUIVALENCE (BANK_MOD5(105),MOD5_HCWID,RE_MOD5_HCWID),              35
     *(NAMES_MOD5(10),NMOD5_HCWID)                                        35
      EQUIVALENCE (COMMEN_MOD5(10),CMOD5_HCWID),(MAP_MOD5(1,10),          35
     *MMOD5_HCWID)                                                        35
      CHARACTER CMOD5_HCWID*40/' '/                                       35
      CHARACTER NMOD5_HCWID*12/'R.HCWID'/                                 35
      INTEGER MMOD5_HCWID(2)/1,1/                                         35
      REAL RE_MOD5_HCWID                                                  35
C -                                                                       35
      REAL MOD5_HCLEN                                                     35
      EQUIVALENCE (BANK_MOD5(106),MOD5_HCLEN,RE_MOD5_HCLEN),              35
     *(NAMES_MOD5(11),NMOD5_HCLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(11),CMOD5_HCLEN),(MAP_MOD5(1,11),          35
     *MMOD5_HCLEN)                                                        35
      CHARACTER CMOD5_HCLEN*40/' '/                                       35
      CHARACTER NMOD5_HCLEN*12/'R.HCLEN'/                                 35
      INTEGER MMOD5_HCLEN(2)/1,1/                                         35
      REAL RE_MOD5_HCLEN                                                  35
C -                                                                       35
      REAL MOD5_PCBHGT                                                    35
      EQUIVALENCE (BANK_MOD5(107),MOD5_PCBHGT,RE_MOD5_PCBHGT),            35
     *(NAMES_MOD5(12),NMOD5_PCBHGT)                                       35
      EQUIVALENCE (COMMEN_MOD5(12),CMOD5_PCBHGT),(MAP_MOD5(1,12),         35
     *MMOD5_PCBHGT)                                                       35
      CHARACTER CMOD5_PCBHGT*40/' '/                                      35
      CHARACTER NMOD5_PCBHGT*12/'R.PCBHGT'/                               35
      INTEGER MMOD5_PCBHGT(2)/1,1/                                        35
      REAL RE_MOD5_PCBHGT                                                 35
C -                                                                       35
      REAL MOD5_PCBWID                                                    35
      EQUIVALENCE (BANK_MOD5(108),MOD5_PCBWID,RE_MOD5_PCBWID),            35
     *(NAMES_MOD5(13),NMOD5_PCBWID)                                       35
      EQUIVALENCE (COMMEN_MOD5(13),CMOD5_PCBWID),(MAP_MOD5(1,13),         35
     *MMOD5_PCBWID)                                                       35
      CHARACTER CMOD5_PCBWID*40/' '/                                      35
      CHARACTER NMOD5_PCBWID*12/'R.PCBWID'/                               35
      INTEGER MMOD5_PCBWID(2)/1,1/                                        35
      REAL RE_MOD5_PCBWID                                                 35
C -                                                                       35
      REAL MOD5_PCBLEN                                                    35
      EQUIVALENCE (BANK_MOD5(109),MOD5_PCBLEN,RE_MOD5_PCBLEN),            35
     *(NAMES_MOD5(14),NMOD5_PCBLEN)                                       35
      EQUIVALENCE (COMMEN_MOD5(14),CMOD5_PCBLEN),(MAP_MOD5(1,14),         35
     *MMOD5_PCBLEN)                                                       35
      CHARACTER CMOD5_PCBLEN*40/' '/                                      35
      CHARACTER NMOD5_PCBLEN*12/'R.PCBLEN'/                               35
      INTEGER MMOD5_PCBLEN(2)/1,1/                                        35
      REAL RE_MOD5_PCBLEN                                                 35
C -                                                                       35
      REAL MOD5_MYHGT                                                     35
      EQUIVALENCE (BANK_MOD5(110),MOD5_MYHGT,RE_MOD5_MYHGT),              35
     *(NAMES_MOD5(15),NMOD5_MYHGT)                                        35
      EQUIVALENCE (COMMEN_MOD5(15),CMOD5_MYHGT),(MAP_MOD5(1,15),          35
     *MMOD5_MYHGT)                                                        35
      CHARACTER CMOD5_MYHGT*40/' '/                                       35
      CHARACTER NMOD5_MYHGT*12/'R.MYHGT'/                                 35
      INTEGER MMOD5_MYHGT(2)/1,1/                                         35
      REAL RE_MOD5_MYHGT                                                  35
C -                                                                       35
      REAL MOD5_MYWID                                                     35
      EQUIVALENCE (BANK_MOD5(111),MOD5_MYWID,RE_MOD5_MYWID),              35
     *(NAMES_MOD5(16),NMOD5_MYWID)                                        35
      EQUIVALENCE (COMMEN_MOD5(16),CMOD5_MYWID),(MAP_MOD5(1,16),          35
     *MMOD5_MYWID)                                                        35
      CHARACTER CMOD5_MYWID*40/' '/                                       35
      CHARACTER NMOD5_MYWID*12/'R.MYWID'/                                 35
      INTEGER MMOD5_MYWID(2)/1,1/                                         35
      REAL RE_MOD5_MYWID                                                  35
C -                                                                       35
      REAL MOD5_MYLEN                                                     35
      EQUIVALENCE (BANK_MOD5(112),MOD5_MYLEN,RE_MOD5_MYLEN),              35
     *(NAMES_MOD5(17),NMOD5_MYLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(17),CMOD5_MYLEN),(MAP_MOD5(1,17),          35
     *MMOD5_MYLEN)                                                        35
      CHARACTER CMOD5_MYLEN*40/' '/                                       35
      CHARACTER NMOD5_MYLEN*12/'R.MYLEN'/                                 35
      INTEGER MMOD5_MYLEN(2)/1,1/                                         35
      REAL RE_MOD5_MYLEN                                                  35
C -                                                                       35
      REAL MOD5_GRHGT                                                     35
      EQUIVALENCE (BANK_MOD5(113),MOD5_GRHGT,RE_MOD5_GRHGT),              35
     *(NAMES_MOD5(18),NMOD5_GRHGT)                                        35
      EQUIVALENCE (COMMEN_MOD5(18),CMOD5_GRHGT),(MAP_MOD5(1,18),          35
     *MMOD5_GRHGT)                                                        35
      CHARACTER CMOD5_GRHGT*40/' '/                                       35
      CHARACTER NMOD5_GRHGT*12/'R.GRHGT'/                                 35
      INTEGER MMOD5_GRHGT(2)/1,1/                                         35
      REAL RE_MOD5_GRHGT                                                  35
C -                                                                       35
      REAL MOD5_GRWID                                                     35
      EQUIVALENCE (BANK_MOD5(114),MOD5_GRWID,RE_MOD5_GRWID),              35
     *(NAMES_MOD5(19),NMOD5_GRWID)                                        35
      EQUIVALENCE (COMMEN_MOD5(19),CMOD5_GRWID),(MAP_MOD5(1,19),          35
     *MMOD5_GRWID)                                                        35
      CHARACTER CMOD5_GRWID*40/' '/                                       35
      CHARACTER NMOD5_GRWID*12/'R.GRWID'/                                 35
      INTEGER MMOD5_GRWID(2)/1,1/                                         35
      REAL RE_MOD5_GRWID                                                  35
C -                                                                       35
      REAL MOD5_GRLEN                                                     35
      EQUIVALENCE (BANK_MOD5(115),MOD5_GRLEN,RE_MOD5_GRLEN),              35
     *(NAMES_MOD5(20),NMOD5_GRLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(20),CMOD5_GRLEN),(MAP_MOD5(1,20),          35
     *MMOD5_GRLEN)                                                        35
      CHARACTER CMOD5_GRLEN*40/' '/                                       35
      CHARACTER NMOD5_GRLEN*12/'R.GRLEN'/                                 35
      INTEGER MMOD5_GRLEN(2)/1,1/                                         35
      REAL RE_MOD5_GRLEN                                                  35
C -                                                                       35
      REAL MOD5_OGHGT                                                     35
      EQUIVALENCE (BANK_MOD5(116),MOD5_OGHGT,RE_MOD5_OGHGT),              35
     *(NAMES_MOD5(21),NMOD5_OGHGT)                                        35
      EQUIVALENCE (COMMEN_MOD5(21),CMOD5_OGHGT),(MAP_MOD5(1,21),          35
     *MMOD5_OGHGT)                                                        35
      CHARACTER CMOD5_OGHGT*40/' '/                                       35
      CHARACTER NMOD5_OGHGT*12/'R.OGHGT'/                                 35
      INTEGER MMOD5_OGHGT(2)/1,1/                                         35
      REAL RE_MOD5_OGHGT                                                  35
C -                                                                       35
      REAL MOD5_OGWID                                                     35
      EQUIVALENCE (BANK_MOD5(117),MOD5_OGWID,RE_MOD5_OGWID),              35
     *(NAMES_MOD5(22),NMOD5_OGWID)                                        35
      EQUIVALENCE (COMMEN_MOD5(22),CMOD5_OGWID),(MAP_MOD5(1,22),          35
     *MMOD5_OGWID)                                                        35
      CHARACTER CMOD5_OGWID*40/' '/                                       35
      CHARACTER NMOD5_OGWID*12/'R.OGWID'/                                 35
      INTEGER MMOD5_OGWID(2)/1,1/                                         35
      REAL RE_MOD5_OGWID                                                  35
C -                                                                       35
      REAL MOD5_OGLEN                                                     35
      EQUIVALENCE (BANK_MOD5(118),MOD5_OGLEN,RE_MOD5_OGLEN),              35
     *(NAMES_MOD5(23),NMOD5_OGLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(23),CMOD5_OGLEN),(MAP_MOD5(1,23),          35
     *MMOD5_OGLEN)                                                        35
      CHARACTER CMOD5_OGLEN*40/' '/                                       35
      CHARACTER NMOD5_OGLEN*12/'R.OGLEN'/                                 35
      INTEGER MMOD5_OGLEN(2)/1,1/                                         35
      REAL RE_MOD5_OGLEN                                                  35
C -                                                                       35
      REAL MOD5_IGHGT                                                     35
      EQUIVALENCE (BANK_MOD5(119),MOD5_IGHGT,RE_MOD5_IGHGT),              35
     *(NAMES_MOD5(24),NMOD5_IGHGT)                                        35
      EQUIVALENCE (COMMEN_MOD5(24),CMOD5_IGHGT),(MAP_MOD5(1,24),          35
     *MMOD5_IGHGT)                                                        35
      CHARACTER CMOD5_IGHGT*40/' '/                                       35
      CHARACTER NMOD5_IGHGT*12/'R.IGHGT'/                                 35
      INTEGER MMOD5_IGHGT(2)/1,1/                                         35
      REAL RE_MOD5_IGHGT                                                  35
C -                                                                       35
      REAL MOD5_IGWID                                                     35
      EQUIVALENCE (BANK_MOD5(120),MOD5_IGWID,RE_MOD5_IGWID),              35
     *(NAMES_MOD5(25),NMOD5_IGWID)                                        35
      EQUIVALENCE (COMMEN_MOD5(25),CMOD5_IGWID),(MAP_MOD5(1,25),          35
     *MMOD5_IGWID)                                                        35
      CHARACTER CMOD5_IGWID*40/' '/                                       35
      CHARACTER NMOD5_IGWID*12/'R.IGWID'/                                 35
      INTEGER MMOD5_IGWID(2)/1,1/                                         35
      REAL RE_MOD5_IGWID                                                  35
C -                                                                       35
      REAL MOD5_IGLEN                                                     35
      EQUIVALENCE (BANK_MOD5(121),MOD5_IGLEN,RE_MOD5_IGLEN),              35
     *(NAMES_MOD5(26),NMOD5_IGLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(26),CMOD5_IGLEN),(MAP_MOD5(1,26),          35
     *MMOD5_IGLEN)                                                        35
      CHARACTER CMOD5_IGLEN*40/' '/                                       35
      CHARACTER NMOD5_IGLEN*12/'R.IGLEN'/                                 35
      INTEGER MMOD5_IGLEN(2)/1,1/                                         35
      REAL RE_MOD5_IGLEN                                                  35
C -                                                                       35
      REAL MOD5_SPRMIN                                                    35
      EQUIVALENCE (BANK_MOD5(122),MOD5_SPRMIN,RE_MOD5_SPRMIN),            35
     *(NAMES_MOD5(27),NMOD5_SPRMIN)                                       35
      EQUIVALENCE (COMMEN_MOD5(27),CMOD5_SPRMIN),(MAP_MOD5(1,27),         35
     *MMOD5_SPRMIN)                                                       35
      CHARACTER CMOD5_SPRMIN*40/' '/                                      35
      CHARACTER NMOD5_SPRMIN*12/'R.SPRMIN'/                               35
      INTEGER MMOD5_SPRMIN(2)/1,1/                                        35
      REAL RE_MOD5_SPRMIN                                                 35
C -                                                                       35
      REAL MOD5_SPRMAX                                                    35
      EQUIVALENCE (BANK_MOD5(123),MOD5_SPRMAX,RE_MOD5_SPRMAX),            35
     *(NAMES_MOD5(28),NMOD5_SPRMAX)                                       35
      EQUIVALENCE (COMMEN_MOD5(28),CMOD5_SPRMAX),(MAP_MOD5(1,28),         35
     *MMOD5_SPRMAX)                                                       35
      CHARACTER CMOD5_SPRMAX*40/' '/                                      35
      CHARACTER NMOD5_SPRMAX*12/'R.SPRMAX'/                               35
      INTEGER MMOD5_SPRMAX(2)/1,1/                                        35
      REAL RE_MOD5_SPRMAX                                                 35
C -                                                                       35
      REAL MOD5_SPLEN                                                     35
      EQUIVALENCE (BANK_MOD5(124),MOD5_SPLEN,RE_MOD5_SPLEN),              35
     *(NAMES_MOD5(29),NMOD5_SPLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(29),CMOD5_SPLEN),(MAP_MOD5(1,29),          35
     *MMOD5_SPLEN)                                                        35
      CHARACTER CMOD5_SPLEN*40/' '/                                       35
      CHARACTER NMOD5_SPLEN*12/'R.SPLEN'/                                 35
      INTEGER MMOD5_SPLEN(2)/1,1/                                         35
      REAL RE_MOD5_SPLEN                                                  35
C -                                                                       35
      REAL MOD5_WGRMIN                                                    35
      EQUIVALENCE (BANK_MOD5(125),MOD5_WGRMIN,RE_MOD5_WGRMIN),            35
     *(NAMES_MOD5(30),NMOD5_WGRMIN)                                       35
      EQUIVALENCE (COMMEN_MOD5(30),CMOD5_WGRMIN),(MAP_MOD5(1,30),         35
     *MMOD5_WGRMIN)                                                       35
      CHARACTER CMOD5_WGRMIN*40/' '/                                      35
      CHARACTER NMOD5_WGRMIN*12/'R.WGRMIN'/                               35
      INTEGER MMOD5_WGRMIN(2)/1,1/                                        35
      REAL RE_MOD5_WGRMIN                                                 35
C -                                                                       35
      REAL MOD5_WGRMAX                                                    35
      EQUIVALENCE (BANK_MOD5(126),MOD5_WGRMAX,RE_MOD5_WGRMAX),            35
     *(NAMES_MOD5(31),NMOD5_WGRMAX)                                       35
      EQUIVALENCE (COMMEN_MOD5(31),CMOD5_WGRMAX),(MAP_MOD5(1,31),         35
     *MMOD5_WGRMAX)                                                       35
      CHARACTER CMOD5_WGRMAX*40/' '/                                      35
      CHARACTER NMOD5_WGRMAX*12/'R.WGRMAX'/                               35
      INTEGER MMOD5_WGRMAX(2)/1,1/                                        35
      REAL RE_MOD5_WGRMAX                                                 35
C -                                                                       35
      REAL MOD5_WGLEN                                                     35
      EQUIVALENCE (BANK_MOD5(127),MOD5_WGLEN,RE_MOD5_WGLEN),              35
     *(NAMES_MOD5(32),NMOD5_WGLEN)                                        35
      EQUIVALENCE (COMMEN_MOD5(32),CMOD5_WGLEN),(MAP_MOD5(1,32),          35
     *MMOD5_WGLEN)                                                        35
      CHARACTER CMOD5_WGLEN*40/' '/                                       35
      CHARACTER NMOD5_WGLEN*12/'R.WGLEN'/                                 35
      INTEGER MMOD5_WGLEN(2)/1,1/                                         35
      REAL RE_MOD5_WGLEN                                                  35
C -                                                                       35
      REAL MOD5_FEEH                                                      35
      EQUIVALENCE (BANK_MOD5(128),MOD5_FEEH,RE_MOD5_FEEH),                35
     *(NAMES_MOD5(33),NMOD5_FEEH)                                         35
      EQUIVALENCE (COMMEN_MOD5(33),CMOD5_FEEH),(MAP_MOD5(1,33),           35
     *MMOD5_FEEH)                                                         35
      CHARACTER CMOD5_FEEH*40/' '/                                        35
      CHARACTER NMOD5_FEEH*12/'R.FEEH'/                                   35
      INTEGER MMOD5_FEEH(2)/1,1/                                          35
      REAL RE_MOD5_FEEH                                                   35
C -                                                                       35
      REAL MOD5_HBWID                                                     35
      EQUIVALENCE (BANK_MOD5(129),MOD5_HBWID,RE_MOD5_HBWID),              35
     *(NAMES_MOD5(34),NMOD5_HBWID)                                        35
      EQUIVALENCE (COMMEN_MOD5(34),CMOD5_HBWID),(MAP_MOD5(1,34),          35
     *MMOD5_HBWID)                                                        35
      CHARACTER CMOD5_HBWID*40/' '/                                       35
      CHARACTER NMOD5_HBWID*12/'R.HBWID'/                                 35
      INTEGER MMOD5_HBWID(2)/1,1/                                         35
      REAL RE_MOD5_HBWID                                                  35
C -                                                                       35
      REAL MOD5_NGAP                                                      35
      EQUIVALENCE (BANK_MOD5(130),MOD5_NGAP,RE_MOD5_NGAP),                35
     *(NAMES_MOD5(35),NMOD5_NGAP)                                         35
      EQUIVALENCE (COMMEN_MOD5(35),CMOD5_NGAP),(MAP_MOD5(1,35),           35
     *MMOD5_NGAP)                                                         35
      CHARACTER CMOD5_NGAP*40/' '/                                        35
      CHARACTER NMOD5_NGAP*12/'R.NGAP'/                                   35
      INTEGER MMOD5_NGAP(2)/1,1/                                          35
      REAL RE_MOD5_NGAP                                                   35
C -                                                                       35
      REAL MOD5_TRAYEDGEZ                                                 35
      EQUIVALENCE (BANK_MOD5(131),MOD5_TRAYEDGEZ,RE_MOD5_TRAYEDGEZ),      35
     *(NAMES_MOD5(36),NMOD5_TRAYEDGEZ)                                    35
      EQUIVALENCE (COMMEN_MOD5(36),CMOD5_TRAYEDGEZ),(MAP_MOD5(1,36),      35
     *MMOD5_TRAYEDGEZ)                                                    35
      CHARACTER CMOD5_TRAYEDGEZ*40/' '/                                   35
      CHARACTER NMOD5_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         35
      INTEGER MMOD5_TRAYEDGEZ(2)/1,1/                                     35
      REAL RE_MOD5_TRAYEDGEZ                                              35
C -                                                                       35
      INTEGER LEN_MOD5,LLEN_MOD5,LINK_MOD5                                35
      PARAMETER (LEN_MOD5=131)                                            35
      PARAMETER (LLEN_MOD5=36)                                            35
      INTEGER FLAG_MOD5/0/,MAP_MOD5(2,LLEN_MOD5),IDNUM_MOD5(10)           35
      CHARACTER NAMES_MOD5(LLEN_MOD5)*12,COMMEN_MOD5(LLEN_MOD5)*40        35
      DATA IDNUM_MOD5/10*0/                                               35
      REAL BANK_MOD5(LEN_MOD5)                                            35
      CHARACTER*60 BPATH_MOD5/' '/                                        35
      SAVE FLAG_MOD5,MAP_MOD5,IDNUM_MOD5,NAMES_MOD5,COMMEN_MOD5,          35
     *BPATH_MOD5                                                          35
      DATA LINK_MOD5/0/,BANK_MOD5/LEN_MOD5*0/                             35
C *                                                                       35
C -                                                                       39
      REAL MOD7_HEIGHT                                                    39
      EQUIVALENCE (BANK_MOD7(3),MOD7_HEIGHT,RE_MOD7_HEIGHT),              39
     *(NAMES_MOD7(1),NMOD7_HEIGHT)                                        39
      EQUIVALENCE (COMMEN_MOD7(1),CMOD7_HEIGHT),(MAP_MOD7(1,1),           39
     *MMOD7_HEIGHT)                                                       39
      CHARACTER CMOD7_HEIGHT*40/' '/                                      39
      CHARACTER NMOD7_HEIGHT*12/'R.HEIGHT'/                               39
      INTEGER MMOD7_HEIGHT(2)/1,1/                                        39
      REAL RE_MOD7_HEIGHT                                                 39
C -                                                                       39
      REAL MOD7_WIDTH                                                     39
      EQUIVALENCE (BANK_MOD7(4),MOD7_WIDTH,RE_MOD7_WIDTH),                39
     *(NAMES_MOD7(2),NMOD7_WIDTH)                                         39
      EQUIVALENCE (COMMEN_MOD7(2),CMOD7_WIDTH),(MAP_MOD7(1,2),            39
     *MMOD7_WIDTH)                                                        39
      CHARACTER CMOD7_WIDTH*40/' '/                                       39
      CHARACTER NMOD7_WIDTH*12/'R.WIDTH'/                                 39
      INTEGER MMOD7_WIDTH(2)/1,1/                                         39
      REAL RE_MOD7_WIDTH                                                  39
C -                                                                       39
      REAL MOD7_LENGTH                                                    39
      EQUIVALENCE (BANK_MOD7(5),MOD7_LENGTH,RE_MOD7_LENGTH),              39
     *(NAMES_MOD7(3),NMOD7_LENGTH)                                        39
      EQUIVALENCE (COMMEN_MOD7(3),CMOD7_LENGTH),(MAP_MOD7(1,3),           39
     *MMOD7_LENGTH)                                                       39
      CHARACTER CMOD7_LENGTH*40/' '/                                      39
      CHARACTER NMOD7_LENGTH*12/'R.LENGTH'/                               39
      INTEGER MMOD7_LENGTH(2)/1,1/                                        39
      REAL RE_MOD7_LENGTH                                                 39
C -                                                                       39
      REAL MOD7_CENTER                                                    39
      EQUIVALENCE (BANK_MOD7(6),MOD7_CENTER,RE_MOD7_CENTER),              39
     *(NAMES_MOD7(4),NMOD7_CENTER)                                        39
      EQUIVALENCE (COMMEN_MOD7(4),CMOD7_CENTER),(MAP_MOD7(1,4),           39
     *MMOD7_CENTER)                                                       39
      CHARACTER CMOD7_CENTER*40/' '/                                      39
      CHARACTER NMOD7_CENTER*12/'R.CENTER'/                               39
      INTEGER MMOD7_CENTER(2)/1,1/                                        39
      REAL RE_MOD7_CENTER                                                 39
C -                                                                       39
      REAL MOD7_MRPCX(32)                                                 39
      EQUIVALENCE (BANK_MOD7(7),MOD7_MRPCX,RE_MOD7_MRPCX),                39
     *(NAMES_MOD7(5),NMOD7_MRPCX)                                         39
      EQUIVALENCE (COMMEN_MOD7(5),CMOD7_MRPCX),(MAP_MOD7(1,5),            39
     *MMOD7_MRPCX)                                                        39
      CHARACTER CMOD7_MRPCX*40/' '/                                       39
      CHARACTER NMOD7_MRPCX*12/'R.MRPCX'/                                 39
      INTEGER MMOD7_MRPCX(2)/32,1/                                        39
      REAL RE_MOD7_MRPCX                                                  39
C -                                                                       39
      REAL MOD7_MRPCZ(32)                                                 39
      EQUIVALENCE (BANK_MOD7(39),MOD7_MRPCZ,RE_MOD7_MRPCZ),               39
     *(NAMES_MOD7(6),NMOD7_MRPCZ)                                         39
      EQUIVALENCE (COMMEN_MOD7(6),CMOD7_MRPCZ),(MAP_MOD7(1,6),            39
     *MMOD7_MRPCZ)                                                        39
      CHARACTER CMOD7_MRPCZ*40/' '/                                       39
      CHARACTER NMOD7_MRPCZ*12/'R.MRPCZ'/                                 39
      INTEGER MMOD7_MRPCZ(2)/32,1/                                        39
      REAL RE_MOD7_MRPCZ                                                  39
C -                                                                       39
      REAL MOD7_MRPCA(32)                                                 39
      EQUIVALENCE (BANK_MOD7(71),MOD7_MRPCA,RE_MOD7_MRPCA),               39
     *(NAMES_MOD7(7),NMOD7_MRPCA)                                         39
      EQUIVALENCE (COMMEN_MOD7(7),CMOD7_MRPCA),(MAP_MOD7(1,7),            39
     *MMOD7_MRPCA)                                                        39
      CHARACTER CMOD7_MRPCA*40/' '/                                       39
      CHARACTER NMOD7_MRPCA*12/'R.MRPCA'/                                 39
      INTEGER MMOD7_MRPCA(2)/32,1/                                        39
      REAL RE_MOD7_MRPCA                                                  39
C -                                                                       39
      REAL MOD7_X0OFFSET                                                  39
      EQUIVALENCE (BANK_MOD7(103),MOD7_X0OFFSET,RE_MOD7_X0OFFSET),        39
     *(NAMES_MOD7(8),NMOD7_X0OFFSET)                                      39
      EQUIVALENCE (COMMEN_MOD7(8),CMOD7_X0OFFSET),(MAP_MOD7(1,8),         39
     *MMOD7_X0OFFSET)                                                     39
      CHARACTER CMOD7_X0OFFSET*40/' '/                                    39
      CHARACTER NMOD7_X0OFFSET*12/'R.X0OFFSET'/                           39
      INTEGER MMOD7_X0OFFSET(2)/1,1/                                      39
      REAL RE_MOD7_X0OFFSET                                               39
C -                                                                       39
      REAL MOD7_HCHGT                                                     39
      EQUIVALENCE (BANK_MOD7(104),MOD7_HCHGT,RE_MOD7_HCHGT),              39
     *(NAMES_MOD7(9),NMOD7_HCHGT)                                         39
      EQUIVALENCE (COMMEN_MOD7(9),CMOD7_HCHGT),(MAP_MOD7(1,9),            39
     *MMOD7_HCHGT)                                                        39
      CHARACTER CMOD7_HCHGT*40/' '/                                       39
      CHARACTER NMOD7_HCHGT*12/'R.HCHGT'/                                 39
      INTEGER MMOD7_HCHGT(2)/1,1/                                         39
      REAL RE_MOD7_HCHGT                                                  39
C -                                                                       39
      REAL MOD7_HCWID                                                     39
      EQUIVALENCE (BANK_MOD7(105),MOD7_HCWID,RE_MOD7_HCWID),              39
     *(NAMES_MOD7(10),NMOD7_HCWID)                                        39
      EQUIVALENCE (COMMEN_MOD7(10),CMOD7_HCWID),(MAP_MOD7(1,10),          39
     *MMOD7_HCWID)                                                        39
      CHARACTER CMOD7_HCWID*40/' '/                                       39
      CHARACTER NMOD7_HCWID*12/'R.HCWID'/                                 39
      INTEGER MMOD7_HCWID(2)/1,1/                                         39
      REAL RE_MOD7_HCWID                                                  39
C -                                                                       39
      REAL MOD7_HCLEN                                                     39
      EQUIVALENCE (BANK_MOD7(106),MOD7_HCLEN,RE_MOD7_HCLEN),              39
     *(NAMES_MOD7(11),NMOD7_HCLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(11),CMOD7_HCLEN),(MAP_MOD7(1,11),          39
     *MMOD7_HCLEN)                                                        39
      CHARACTER CMOD7_HCLEN*40/' '/                                       39
      CHARACTER NMOD7_HCLEN*12/'R.HCLEN'/                                 39
      INTEGER MMOD7_HCLEN(2)/1,1/                                         39
      REAL RE_MOD7_HCLEN                                                  39
C -                                                                       39
      REAL MOD7_PCBHGT                                                    39
      EQUIVALENCE (BANK_MOD7(107),MOD7_PCBHGT,RE_MOD7_PCBHGT),            39
     *(NAMES_MOD7(12),NMOD7_PCBHGT)                                       39
      EQUIVALENCE (COMMEN_MOD7(12),CMOD7_PCBHGT),(MAP_MOD7(1,12),         39
     *MMOD7_PCBHGT)                                                       39
      CHARACTER CMOD7_PCBHGT*40/' '/                                      39
      CHARACTER NMOD7_PCBHGT*12/'R.PCBHGT'/                               39
      INTEGER MMOD7_PCBHGT(2)/1,1/                                        39
      REAL RE_MOD7_PCBHGT                                                 39
C -                                                                       39
      REAL MOD7_PCBWID                                                    39
      EQUIVALENCE (BANK_MOD7(108),MOD7_PCBWID,RE_MOD7_PCBWID),            39
     *(NAMES_MOD7(13),NMOD7_PCBWID)                                       39
      EQUIVALENCE (COMMEN_MOD7(13),CMOD7_PCBWID),(MAP_MOD7(1,13),         39
     *MMOD7_PCBWID)                                                       39
      CHARACTER CMOD7_PCBWID*40/' '/                                      39
      CHARACTER NMOD7_PCBWID*12/'R.PCBWID'/                               39
      INTEGER MMOD7_PCBWID(2)/1,1/                                        39
      REAL RE_MOD7_PCBWID                                                 39
C -                                                                       39
      REAL MOD7_PCBLEN                                                    39
      EQUIVALENCE (BANK_MOD7(109),MOD7_PCBLEN,RE_MOD7_PCBLEN),            39
     *(NAMES_MOD7(14),NMOD7_PCBLEN)                                       39
      EQUIVALENCE (COMMEN_MOD7(14),CMOD7_PCBLEN),(MAP_MOD7(1,14),         39
     *MMOD7_PCBLEN)                                                       39
      CHARACTER CMOD7_PCBLEN*40/' '/                                      39
      CHARACTER NMOD7_PCBLEN*12/'R.PCBLEN'/                               39
      INTEGER MMOD7_PCBLEN(2)/1,1/                                        39
      REAL RE_MOD7_PCBLEN                                                 39
C -                                                                       39
      REAL MOD7_MYHGT                                                     39
      EQUIVALENCE (BANK_MOD7(110),MOD7_MYHGT,RE_MOD7_MYHGT),              39
     *(NAMES_MOD7(15),NMOD7_MYHGT)                                        39
      EQUIVALENCE (COMMEN_MOD7(15),CMOD7_MYHGT),(MAP_MOD7(1,15),          39
     *MMOD7_MYHGT)                                                        39
      CHARACTER CMOD7_MYHGT*40/' '/                                       39
      CHARACTER NMOD7_MYHGT*12/'R.MYHGT'/                                 39
      INTEGER MMOD7_MYHGT(2)/1,1/                                         39
      REAL RE_MOD7_MYHGT                                                  39
C -                                                                       39
      REAL MOD7_MYWID                                                     39
      EQUIVALENCE (BANK_MOD7(111),MOD7_MYWID,RE_MOD7_MYWID),              39
     *(NAMES_MOD7(16),NMOD7_MYWID)                                        39
      EQUIVALENCE (COMMEN_MOD7(16),CMOD7_MYWID),(MAP_MOD7(1,16),          39
     *MMOD7_MYWID)                                                        39
      CHARACTER CMOD7_MYWID*40/' '/                                       39
      CHARACTER NMOD7_MYWID*12/'R.MYWID'/                                 39
      INTEGER MMOD7_MYWID(2)/1,1/                                         39
      REAL RE_MOD7_MYWID                                                  39
C -                                                                       39
      REAL MOD7_MYLEN                                                     39
      EQUIVALENCE (BANK_MOD7(112),MOD7_MYLEN,RE_MOD7_MYLEN),              39
     *(NAMES_MOD7(17),NMOD7_MYLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(17),CMOD7_MYLEN),(MAP_MOD7(1,17),          39
     *MMOD7_MYLEN)                                                        39
      CHARACTER CMOD7_MYLEN*40/' '/                                       39
      CHARACTER NMOD7_MYLEN*12/'R.MYLEN'/                                 39
      INTEGER MMOD7_MYLEN(2)/1,1/                                         39
      REAL RE_MOD7_MYLEN                                                  39
C -                                                                       39
      REAL MOD7_GRHGT                                                     39
      EQUIVALENCE (BANK_MOD7(113),MOD7_GRHGT,RE_MOD7_GRHGT),              39
     *(NAMES_MOD7(18),NMOD7_GRHGT)                                        39
      EQUIVALENCE (COMMEN_MOD7(18),CMOD7_GRHGT),(MAP_MOD7(1,18),          39
     *MMOD7_GRHGT)                                                        39
      CHARACTER CMOD7_GRHGT*40/' '/                                       39
      CHARACTER NMOD7_GRHGT*12/'R.GRHGT'/                                 39
      INTEGER MMOD7_GRHGT(2)/1,1/                                         39
      REAL RE_MOD7_GRHGT                                                  39
C -                                                                       39
      REAL MOD7_GRWID                                                     39
      EQUIVALENCE (BANK_MOD7(114),MOD7_GRWID,RE_MOD7_GRWID),              39
     *(NAMES_MOD7(19),NMOD7_GRWID)                                        39
      EQUIVALENCE (COMMEN_MOD7(19),CMOD7_GRWID),(MAP_MOD7(1,19),          39
     *MMOD7_GRWID)                                                        39
      CHARACTER CMOD7_GRWID*40/' '/                                       39
      CHARACTER NMOD7_GRWID*12/'R.GRWID'/                                 39
      INTEGER MMOD7_GRWID(2)/1,1/                                         39
      REAL RE_MOD7_GRWID                                                  39
C -                                                                       39
      REAL MOD7_GRLEN                                                     39
      EQUIVALENCE (BANK_MOD7(115),MOD7_GRLEN,RE_MOD7_GRLEN),              39
     *(NAMES_MOD7(20),NMOD7_GRLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(20),CMOD7_GRLEN),(MAP_MOD7(1,20),          39
     *MMOD7_GRLEN)                                                        39
      CHARACTER CMOD7_GRLEN*40/' '/                                       39
      CHARACTER NMOD7_GRLEN*12/'R.GRLEN'/                                 39
      INTEGER MMOD7_GRLEN(2)/1,1/                                         39
      REAL RE_MOD7_GRLEN                                                  39
C -                                                                       39
      REAL MOD7_OGHGT                                                     39
      EQUIVALENCE (BANK_MOD7(116),MOD7_OGHGT,RE_MOD7_OGHGT),              39
     *(NAMES_MOD7(21),NMOD7_OGHGT)                                        39
      EQUIVALENCE (COMMEN_MOD7(21),CMOD7_OGHGT),(MAP_MOD7(1,21),          39
     *MMOD7_OGHGT)                                                        39
      CHARACTER CMOD7_OGHGT*40/' '/                                       39
      CHARACTER NMOD7_OGHGT*12/'R.OGHGT'/                                 39
      INTEGER MMOD7_OGHGT(2)/1,1/                                         39
      REAL RE_MOD7_OGHGT                                                  39
C -                                                                       39
      REAL MOD7_OGWID                                                     39
      EQUIVALENCE (BANK_MOD7(117),MOD7_OGWID,RE_MOD7_OGWID),              39
     *(NAMES_MOD7(22),NMOD7_OGWID)                                        39
      EQUIVALENCE (COMMEN_MOD7(22),CMOD7_OGWID),(MAP_MOD7(1,22),          39
     *MMOD7_OGWID)                                                        39
      CHARACTER CMOD7_OGWID*40/' '/                                       39
      CHARACTER NMOD7_OGWID*12/'R.OGWID'/                                 39
      INTEGER MMOD7_OGWID(2)/1,1/                                         39
      REAL RE_MOD7_OGWID                                                  39
C -                                                                       39
      REAL MOD7_OGLEN                                                     39
      EQUIVALENCE (BANK_MOD7(118),MOD7_OGLEN,RE_MOD7_OGLEN),              39
     *(NAMES_MOD7(23),NMOD7_OGLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(23),CMOD7_OGLEN),(MAP_MOD7(1,23),          39
     *MMOD7_OGLEN)                                                        39
      CHARACTER CMOD7_OGLEN*40/' '/                                       39
      CHARACTER NMOD7_OGLEN*12/'R.OGLEN'/                                 39
      INTEGER MMOD7_OGLEN(2)/1,1/                                         39
      REAL RE_MOD7_OGLEN                                                  39
C -                                                                       39
      REAL MOD7_IGHGT                                                     39
      EQUIVALENCE (BANK_MOD7(119),MOD7_IGHGT,RE_MOD7_IGHGT),              39
     *(NAMES_MOD7(24),NMOD7_IGHGT)                                        39
      EQUIVALENCE (COMMEN_MOD7(24),CMOD7_IGHGT),(MAP_MOD7(1,24),          39
     *MMOD7_IGHGT)                                                        39
      CHARACTER CMOD7_IGHGT*40/' '/                                       39
      CHARACTER NMOD7_IGHGT*12/'R.IGHGT'/                                 39
      INTEGER MMOD7_IGHGT(2)/1,1/                                         39
      REAL RE_MOD7_IGHGT                                                  39
C -                                                                       39
      REAL MOD7_IGWID                                                     39
      EQUIVALENCE (BANK_MOD7(120),MOD7_IGWID,RE_MOD7_IGWID),              39
     *(NAMES_MOD7(25),NMOD7_IGWID)                                        39
      EQUIVALENCE (COMMEN_MOD7(25),CMOD7_IGWID),(MAP_MOD7(1,25),          39
     *MMOD7_IGWID)                                                        39
      CHARACTER CMOD7_IGWID*40/' '/                                       39
      CHARACTER NMOD7_IGWID*12/'R.IGWID'/                                 39
      INTEGER MMOD7_IGWID(2)/1,1/                                         39
      REAL RE_MOD7_IGWID                                                  39
C -                                                                       39
      REAL MOD7_IGLEN                                                     39
      EQUIVALENCE (BANK_MOD7(121),MOD7_IGLEN,RE_MOD7_IGLEN),              39
     *(NAMES_MOD7(26),NMOD7_IGLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(26),CMOD7_IGLEN),(MAP_MOD7(1,26),          39
     *MMOD7_IGLEN)                                                        39
      CHARACTER CMOD7_IGLEN*40/' '/                                       39
      CHARACTER NMOD7_IGLEN*12/'R.IGLEN'/                                 39
      INTEGER MMOD7_IGLEN(2)/1,1/                                         39
      REAL RE_MOD7_IGLEN                                                  39
C -                                                                       39
      REAL MOD7_SPRMIN                                                    39
      EQUIVALENCE (BANK_MOD7(122),MOD7_SPRMIN,RE_MOD7_SPRMIN),            39
     *(NAMES_MOD7(27),NMOD7_SPRMIN)                                       39
      EQUIVALENCE (COMMEN_MOD7(27),CMOD7_SPRMIN),(MAP_MOD7(1,27),         39
     *MMOD7_SPRMIN)                                                       39
      CHARACTER CMOD7_SPRMIN*40/' '/                                      39
      CHARACTER NMOD7_SPRMIN*12/'R.SPRMIN'/                               39
      INTEGER MMOD7_SPRMIN(2)/1,1/                                        39
      REAL RE_MOD7_SPRMIN                                                 39
C -                                                                       39
      REAL MOD7_SPRMAX                                                    39
      EQUIVALENCE (BANK_MOD7(123),MOD7_SPRMAX,RE_MOD7_SPRMAX),            39
     *(NAMES_MOD7(28),NMOD7_SPRMAX)                                       39
      EQUIVALENCE (COMMEN_MOD7(28),CMOD7_SPRMAX),(MAP_MOD7(1,28),         39
     *MMOD7_SPRMAX)                                                       39
      CHARACTER CMOD7_SPRMAX*40/' '/                                      39
      CHARACTER NMOD7_SPRMAX*12/'R.SPRMAX'/                               39
      INTEGER MMOD7_SPRMAX(2)/1,1/                                        39
      REAL RE_MOD7_SPRMAX                                                 39
C -                                                                       39
      REAL MOD7_SPLEN                                                     39
      EQUIVALENCE (BANK_MOD7(124),MOD7_SPLEN,RE_MOD7_SPLEN),              39
     *(NAMES_MOD7(29),NMOD7_SPLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(29),CMOD7_SPLEN),(MAP_MOD7(1,29),          39
     *MMOD7_SPLEN)                                                        39
      CHARACTER CMOD7_SPLEN*40/' '/                                       39
      CHARACTER NMOD7_SPLEN*12/'R.SPLEN'/                                 39
      INTEGER MMOD7_SPLEN(2)/1,1/                                         39
      REAL RE_MOD7_SPLEN                                                  39
C -                                                                       39
      REAL MOD7_WGRMIN                                                    39
      EQUIVALENCE (BANK_MOD7(125),MOD7_WGRMIN,RE_MOD7_WGRMIN),            39
     *(NAMES_MOD7(30),NMOD7_WGRMIN)                                       39
      EQUIVALENCE (COMMEN_MOD7(30),CMOD7_WGRMIN),(MAP_MOD7(1,30),         39
     *MMOD7_WGRMIN)                                                       39
      CHARACTER CMOD7_WGRMIN*40/' '/                                      39
      CHARACTER NMOD7_WGRMIN*12/'R.WGRMIN'/                               39
      INTEGER MMOD7_WGRMIN(2)/1,1/                                        39
      REAL RE_MOD7_WGRMIN                                                 39
C -                                                                       39
      REAL MOD7_WGRMAX                                                    39
      EQUIVALENCE (BANK_MOD7(126),MOD7_WGRMAX,RE_MOD7_WGRMAX),            39
     *(NAMES_MOD7(31),NMOD7_WGRMAX)                                       39
      EQUIVALENCE (COMMEN_MOD7(31),CMOD7_WGRMAX),(MAP_MOD7(1,31),         39
     *MMOD7_WGRMAX)                                                       39
      CHARACTER CMOD7_WGRMAX*40/' '/                                      39
      CHARACTER NMOD7_WGRMAX*12/'R.WGRMAX'/                               39
      INTEGER MMOD7_WGRMAX(2)/1,1/                                        39
      REAL RE_MOD7_WGRMAX                                                 39
C -                                                                       39
      REAL MOD7_WGLEN                                                     39
      EQUIVALENCE (BANK_MOD7(127),MOD7_WGLEN,RE_MOD7_WGLEN),              39
     *(NAMES_MOD7(32),NMOD7_WGLEN)                                        39
      EQUIVALENCE (COMMEN_MOD7(32),CMOD7_WGLEN),(MAP_MOD7(1,32),          39
     *MMOD7_WGLEN)                                                        39
      CHARACTER CMOD7_WGLEN*40/' '/                                       39
      CHARACTER NMOD7_WGLEN*12/'R.WGLEN'/                                 39
      INTEGER MMOD7_WGLEN(2)/1,1/                                         39
      REAL RE_MOD7_WGLEN                                                  39
C -                                                                       39
      REAL MOD7_FEEH                                                      39
      EQUIVALENCE (BANK_MOD7(128),MOD7_FEEH,RE_MOD7_FEEH),                39
     *(NAMES_MOD7(33),NMOD7_FEEH)                                         39
      EQUIVALENCE (COMMEN_MOD7(33),CMOD7_FEEH),(MAP_MOD7(1,33),           39
     *MMOD7_FEEH)                                                         39
      CHARACTER CMOD7_FEEH*40/' '/                                        39
      CHARACTER NMOD7_FEEH*12/'R.FEEH'/                                   39
      INTEGER MMOD7_FEEH(2)/1,1/                                          39
      REAL RE_MOD7_FEEH                                                   39
C -                                                                       39
      REAL MOD7_HBWID                                                     39
      EQUIVALENCE (BANK_MOD7(129),MOD7_HBWID,RE_MOD7_HBWID),              39
     *(NAMES_MOD7(34),NMOD7_HBWID)                                        39
      EQUIVALENCE (COMMEN_MOD7(34),CMOD7_HBWID),(MAP_MOD7(1,34),          39
     *MMOD7_HBWID)                                                        39
      CHARACTER CMOD7_HBWID*40/' '/                                       39
      CHARACTER NMOD7_HBWID*12/'R.HBWID'/                                 39
      INTEGER MMOD7_HBWID(2)/1,1/                                         39
      REAL RE_MOD7_HBWID                                                  39
C -                                                                       39
      REAL MOD7_NGAP                                                      39
      EQUIVALENCE (BANK_MOD7(130),MOD7_NGAP,RE_MOD7_NGAP),                39
     *(NAMES_MOD7(35),NMOD7_NGAP)                                         39
      EQUIVALENCE (COMMEN_MOD7(35),CMOD7_NGAP),(MAP_MOD7(1,35),           39
     *MMOD7_NGAP)                                                         39
      CHARACTER CMOD7_NGAP*40/' '/                                        39
      CHARACTER NMOD7_NGAP*12/'R.NGAP'/                                   39
      INTEGER MMOD7_NGAP(2)/1,1/                                          39
      REAL RE_MOD7_NGAP                                                   39
C -                                                                       39
      REAL MOD7_TRAYEDGEZ                                                 39
      EQUIVALENCE (BANK_MOD7(131),MOD7_TRAYEDGEZ,RE_MOD7_TRAYEDGEZ),      39
     *(NAMES_MOD7(36),NMOD7_TRAYEDGEZ)                                    39
      EQUIVALENCE (COMMEN_MOD7(36),CMOD7_TRAYEDGEZ),(MAP_MOD7(1,36),      39
     *MMOD7_TRAYEDGEZ)                                                    39
      CHARACTER CMOD7_TRAYEDGEZ*40/' '/                                   39
      CHARACTER NMOD7_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         39
      INTEGER MMOD7_TRAYEDGEZ(2)/1,1/                                     39
      REAL RE_MOD7_TRAYEDGEZ                                              39
C -                                                                       39
      INTEGER LEN_MOD7,LLEN_MOD7,LINK_MOD7                                39
      PARAMETER (LEN_MOD7=131)                                            39
      PARAMETER (LLEN_MOD7=36)                                            39
      INTEGER FLAG_MOD7/0/,MAP_MOD7(2,LLEN_MOD7),IDNUM_MOD7(10)           39
      CHARACTER NAMES_MOD7(LLEN_MOD7)*12,COMMEN_MOD7(LLEN_MOD7)*40        39
      DATA IDNUM_MOD7/10*0/                                               39
      REAL BANK_MOD7(LEN_MOD7)                                            39
      CHARACTER*60 BPATH_MOD7/' '/                                        39
      SAVE FLAG_MOD7,MAP_MOD7,IDNUM_MOD7,NAMES_MOD7,COMMEN_MOD7,          39
     *BPATH_MOD7                                                          39
      DATA LINK_MOD7/0/,BANK_MOD7/LEN_MOD7*0/                             39
C *                                                                       39
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           41
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        41
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       41
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP,ISLAT, YEAR                       43
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            45
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               45
      BPATH_BTOG='BTOG*'                                                  45
      IDNUM_BTOG(1)=0                                                     46
C GEOMETRY VERSION                                                        46
      BTOG_VERSION=6                                                      46
      CBTOG_VERSION='GEOMETRY VERSION'                                    46
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                47
      BTOG_RMIN=207.80                                                    47
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               47
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           48
      BTOG_RMAX=219.5                                                     48
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          48
C CTB/TOF TUBE HALF LENGTH                                                49
      BTOG_DZ=246.0                                                       49
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 49
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5=1 TRAY-TOFR, 6=F   50
      BTOG_CHOICE=11                                                      50
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5  50
     *=1 TRAY-TOFR, 6=F'                                                  50
C TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN                    51
      BTOG_POSIT1(1)=32                                                   51
      BTOG_POSIT1(2)=33                                                   51
      CBTOG_POSIT1='TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN  51
     *'                                                                   51
C TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN                           52
      BTOG_POSIT2=23                                                      52
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN'        52
C TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 POSN                  53
      BTOG_POSIT3=33                                                      53
      CBTOG_POSIT3='TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 PO  53
     *SN'                                                                 53
C TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST AND WEST TRAYS      54
      BTOG_POSIT4(1)=17                                                   54
      BTOG_POSIT4(2)=18                                                   54
      BTOG_POSIT4(3)=19                                                   54
      BTOG_POSIT4(4)=37                                                   54
      BTOG_POSIT4(5)=39                                                   54
      CBTOG_POSIT4='TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST   54
     *AND WEST TRAYS'                                                     54
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    55
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           55
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   55
C *                                                                       55
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    55
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           55
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   55
C *                                                                       56
      CALL AGSBEGM(MODULE,IPRIN)                                          56
      IF (IPRIN.LT.0) RETURN                                              56
C Filling bank TRAY - GENERAL TRAY STATS                                  56
      BANK_TITLE='GENERAL TRAY STATS'                                     56
      BPATH_TRAY='TRAY*'                                                  56
      IDNUM_TRAY(1)=0                                                     57
C TRAY HEIGHT(8.89)                                                       57
      TRAY_HEIGHT=8.89                                                    57
      CTRAY_HEIGHT='TRAY HEIGHT(8.89)'                                    57
C FULL TRAY WIDTH                                                         58
      TRAY_WIDTH=21.59                                                    58
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       58
C FULL TRAY LENGTH(241.62)                                                59
      TRAY_LENGTH=241.62                                                  59
      CTRAY_LENGTH='FULL TRAY LENGTH(241.62)'                             59
C TRAY WALL THICKNESS                                                     60
      TRAY_WALLTHK=0.13                                                   60
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 60
C SUPPORT HEIGHT (RADIAL)                                                 61
      TRAY_SUPFULLH=2.03                                                  61
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            61
C SUPPORT FULL WIDTH WITH ARMS                                            62
      TRAY_SUPFULLW=15.24                                                 62
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       62
C SUPPORT LENGTH                                                          63
      TRAY_SUPLEN=215.9                                                   63
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       63
C SUPPORT BASE WIDTH                                                      64
      TRAY_SUPBASEW=9.22                                                  64
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 64
C SUPPORT BASE THICKNESS                                                  65
      TRAY_SUPBASET=0.32                                                  65
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             65
C SUPPORT ARM THICKNESS                                                   66
      TRAY_SUPARMT=0.64                                                   66
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               66
C COOLING CHANNEL OUTER RADIUS                                            67
      TRAY_COOLOUTR=0.80                                                  67
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       67
C COOLING CHANNEL INNER RADIUS                                            68
      TRAY_COOLINNR=0.48                                                  68
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       68
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               69
      TRAY_STRIPT=0.08                                                    69
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            69
C FOOT INSET FROM TRAY EDGE                                               70
      TRAY_FOOTINSE=1.06                                                  70
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          70
C THICKNESS OF FOOT MATERIAL                                              71
      TRAY_FOOTTHK=0.23                                                   71
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          71
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               72
      TRAY_FOOT1LEN=1.68                                                  72
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          72
C THICKNESS OF SECOND FOOT SECTION                                        73
      TRAY_FOOT2THK=1.16                                                  73
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   73
C LENGTH OF THIRD PART OF FOOT                                            74
      TRAY_FOOT3LEN=2.16                                                  74
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       74
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    75
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           75
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   75
C *                                                                       75
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    75
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           75
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   75
C *                                                                       76
      CALL AGSBEGM(MODULE,IPRIN)                                          76
      IF (IPRIN.LT.0) RETURN                                              76
C Filling bank CTBB - BARREL TRIGGER STATS                                76
      BANK_TITLE='BARREL TRIGGER STATS'                                   76
      BPATH_CTBB='CTBB*'                                                  76
      IDNUM_CTBB(1)=0                                                     77
C FIRST SLAB (B) LENGTH                                                   77
      CTBB_SLAB1LEN=112.5                                                 77
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              77
C SECOND SLAB (A)LENGTH                                                   78
      CTBB_SLAB2LEN=130.0                                                 78
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              78
C FIRST SLAB (B) X POSITION                                               79
      CTBB_SLAB1X=5.84                                                    79
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            79
C SECOND SLAB (A) X POSITION                                              80
      CTBB_SLAB2X=2.67                                                    80
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           80
C SCINTILLATOR SLAB THICKNESSES                                           81
      CTBB_SLABTHCK=1.0                                                   81
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      81
C SCINTILLATOR SLAB WIDTH                                                 82
      CTBB_SLABWID=21.0                                                   82
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             82
C OPTICAL CONVERTER LENGTH                                                83
      CTBB_CONVLEN=8.5                                                    83
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            83
C OPTICAL CONVERTOR MIN WIDTH                                             84
      CTBB_CONVWIDM=4.0                                                   84
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        84
C OPTICAL CONVERTOR THICKNESS                                             85
      CTBB_CONVTHCK=0.92                                                  85
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        85
C PMT LENGTH                                                              86
      CTBB_PMTLEN=5.0                                                     86
      CCTBB_PMTLEN='PMT LENGTH'                                           86
C PMT MAX RADIUS                                                          87
      CTBB_PMTMAXR=2.0                                                    87
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      87
C PMT MIN RADIUS                                                          88
      CTBB_PMTMINR=1.84                                                   88
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      88
C BASE LENGTH                                                             89
      CTBB_BASELEN=4.0                                                    89
      CCTBB_BASELEN='BASE LENGTH'                                         89
C BASE MAX RADIUS                                                         90
      CTBB_BASEMAXR=2.13                                                  90
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    90
C BASE MIN RADIUS                                                         91
      CTBB_BASEMINR=1.0                                                   91
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    91
C READOUT ELECTRONICS THICKNESS                                           92
      CTBB_ELECTHCK=0.25                                                  92
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      92
C THICKNESS OF TYVEK + BLACK PLASTIC                                      93
      CTBB_WRAP=0.13                                                      93
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     93
C THICKNESS OF SHIM TO POSITION SLAT 2                                    94
      CTBB_SHIM=0.26                                                      94
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   94
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    95
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           95
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   95
C *                                                                       95
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    95
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           95
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   95
C *                                                                       96
      CALL AGSBEGM(MODULE,IPRIN)                                          96
      IF (IPRIN.LT.0) RETURN                                              96
C Filling bank TOFF - TIME OF FLIGHT STATS                                96
      BANK_TITLE='TIME OF FLIGHT STATS'                                   96
      BPATH_TOFF='TOFF*'                                                  96
      IDNUM_TOFF(1)=0                                                     97
C WIDTH OF THE 5W BOX (BMTD)                                              97
      TOFF_BOXWIDTH=21.0875                                               97
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         97
C SLAT LENGTH                                                             98
      TOFF_SLATLEN=20.0                                                   98
      CTOFF_SLATLEN='SLAT LENGTH'                                         98
C 5_WIDE MOTHER BOX Z LENGTH/2.                                           99
      TOFF_SLAT5Z=15.5                                                    99
      CTOFF_SLAT5Z='5_WIDE MOTHER BOX Z LENGTH/2.'                        99
C (5)4_WIDE_SLAT Z POSITION FOR 10 ROWS                                   102
      TOFF_SLATZ(1)=104.938                                               102
      TOFF_SLATZ(2)=84.060                                                102
      TOFF_SLATZ(3)=62.860                                                102
      TOFF_SLATZ(4)=41.254                                                102
      TOFF_SLATZ(5)=18.966                                                102
      TOFF_SLATZ(6)=-3.954                                                102
      TOFF_SLATZ(7)=-27.528                                               102
      TOFF_SLATZ(8)=-51.254                                               102
      TOFF_SLATZ(9)=-75.634                                               102
      TOFF_SLATZ(10)=-100.683                                             102
      CTOFF_SLATZ='(5)4_WIDE_SLAT Z POSITION FOR 10 ROWS'                 102
C SLAT ASSEMBLY X POS                                                     104
      TOFF_SLATX(1)=-1.7                                                  104
      TOFF_SLATX(2)=-0.4                                                  104
      TOFF_SLATX(3)=-0.2                                                  104
      TOFF_SLATX(4)=-0.2                                                  104
      TOFF_SLATX(5)=-0.2                                                  104
      TOFF_SLATX(6)=-0.2                                                  104
      TOFF_SLATX(7)=-0.2                                                  104
      TOFF_SLATX(8)=-0.2                                                  104
      TOFF_SLATX(9)=-0.2                                                  104
      TOFF_SLATX(10)=-0.2                                                 104
      CTOFF_SLATX='SLAT ASSEMBLY X POS'                                   104
C SLAT ASSEMBLY ALPHAY                                                    106
      TOFF_SLATAY(1)=5.0                                                  106
      TOFF_SLATAY(2)=10.0                                                 106
      TOFF_SLATAY(3)=11.0                                                 106
      TOFF_SLATAY(4)=11.0                                                 106
      TOFF_SLATAY(5)=11.0                                                 106
      TOFF_SLATAY(6)=11.0                                                 106
      TOFF_SLATAY(7)=11.0                                                 106
      TOFF_SLATAY(8)=11.0                                                 106
      TOFF_SLATAY(9)=11.0                                                 106
      TOFF_SLATAY(10)=11.0                                                106
      CTOFF_SLATAY='SLAT ASSEMBLY ALPHAY'                                 106
C SCINTILLATOR SLAB THICKNESSES                                           107
      TOFF_SLATTHCK=2.0                                                   107
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      107
C SCINTILLATOR SLAB WIDTH (4.0)                                           108
      TOFF_SLATWID=3.81                                                   108
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       108
C SLAT ASSY. ANGLE W.R.T. TRAY                                            109
      TOFF_SLATANG=11.5                                                   109
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        109
C PMT LENGTH                                                              110
      TOFF_PMTLEN=5.0                                                     110
      CTOFF_PMTLEN='PMT LENGTH'                                           110
C PMT MAX RADIUS                                                          111
      TOFF_PMTMAXR=1.91                                                   111
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      111
C PMT MIN RADIUS                                                          112
      TOFF_PMTMINR=1.8                                                    112
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      112
C THICKNESS OF SOCKET                                                     113
      TOFF_SOCKLEN=1.0                                                    113
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 113
C BASE LENGTH                                                             114
      TOFF_BASELEN=5.0                                                    114
      CTOFF_BASELEN='BASE LENGTH'                                         114
C BASE MAX RADIUS                                                         115
      TOFF_BASEMAXR=1.91                                                  115
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    115
C BASE MIN RADIUS                                                         116
      TOFF_BASEMINR=1.8                                                   116
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    116
C CELL WIDTH                                                              117
      TOFF_CELLWID=3.1                                                    117
      CTOFF_CELLWID='CELL WIDTH'                                          117
C CELL HEIGHT                                                             118
      TOFF_CELLHGT=1.6                                                    118
      CTOFF_CELLHGT='CELL HEIGHT'                                         118
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           119
      TOFF_ELECHGT=3.0                                                    119
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       119
C FEE BOARD THICKNESS (67 MILS)                                           120
      TOFF_ELECTHCK=0.17                                                  120
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      120
C FEE BOARD WIDTH (WAS 21)                                                121
      TOFF_ELECWID=20.3                                                   121
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            121
C FEE BOARD LENGTH (WAS 16)                                               122
      TOFF_ELECLEN=5.715                                                  122
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           122
C FEE Z POSITION FOR 10 ROWS                                              125
      TOFF_ELECZ(1)=105.610                                               125
      TOFF_ELECZ(2)=84.573                                                125
      TOFF_ELECZ(3)=63.224                                                125
      TOFF_ELECZ(4)=41.667                                                125
      TOFF_ELECZ(5)=19.379                                                125
      TOFF_ELECZ(6)=-3.542                                                125
      TOFF_ELECZ(7)=-27.165                                               125
      TOFF_ELECZ(8)=-50.841                                               125
      TOFF_ELECZ(9)=-75.170                                               125
      TOFF_ELECZ(10)=-100.270                                             125
      CTOFF_ELECZ='FEE Z POSITION FOR 10 ROWS'                            125
C PLASTIC ANGLES (BPLA) Z CENTER OFFSET                                   126
      TOFF_PLASPOS=3.0                                                    126
      CTOFF_PLASPOS='PLASTIC ANGLES (BPLA) Z CENTER OFFSET'               126
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
C BCON Y LENGTH FOR TOFP SLAT ASSEMBLY                                    131
      TOFF_BCONYLEN=2.50                                                  131
      CTOFF_BCONYLEN='BCON Y LENGTH FOR TOFP SLAT ASSEMBLY'               131
C BCON Z LENGTH FOR TOFP SLAT ASSEMBLY                                    132
      TOFF_BCONZLEN=1.20                                                  132
      CTOFF_BCONZLEN='BCON Z LENGTH FOR TOFP SLAT ASSEMBLY'               132
C BCON DX FOR BPLA = 0.08*2.54                                            133
      TOFF_BCONPLDX=0.203                                                 133
      CTOFF_BCONPLDX='BCON DX FOR BPLA = 0.08*2.54'                       133
C BCON DZ FOR BPLA = 0.25*2.54                                            134
      TOFF_BCONPLDZ=0.635                                                 134
      CTOFF_BCONPLDZ='BCON DZ FOR BPLA = 0.25*2.54'                       134
C BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0                            135
      TOFF_BLEMPOSX=0.52                                                  135
      CTOFF_BLEMPOSX='BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0'       135
C BLEM POSITION Y IN BFEE                                                 138
      TOFF_BLEMPOSY(1)=-7.0                                               138
      TOFF_BLEMPOSY(2)=-3.5                                               138
      TOFF_BLEMPOSY(3)=0.0                                                138
      TOFF_BLEMPOSY(4)=3.5                                                138
      TOFF_BLEMPOSY(5)=7.0                                                138
      TOFF_BLEMPOSY(6)=-7.0                                               138
      TOFF_BLEMPOSY(7)=-3.5                                               138
      TOFF_BLEMPOSY(8)=0.0                                                138
      TOFF_BLEMPOSY(9)=3.5                                                138
      TOFF_BLEMPOSY(10)=7.0                                               138
      TOFF_BLEMPOSY(11)=-6.0                                              138
      TOFF_BLEMPOSY(12)=-2.5                                              138
      TOFF_BLEMPOSY(13)=1.0                                               138
      TOFF_BLEMPOSY(14)=4.5                                               138
      TOFF_BLEMPOSY(15)=8.0                                               138
      CTOFF_BLEMPOSY='BLEM POSITION Y IN BFEE'                            138
C BLEM POSITION Z1 IN BFEE                                                139
      TOFF_BLEMPOSZ1=2.0                                                  139
      CTOFF_BLEMPOSZ1='BLEM POSITION Z1 IN BFEE'                          139
C BLEM POSITION Z2 IN BFEE                                                140
      TOFF_BLEMPOSZ2=-2.0                                                 140
      CTOFF_BLEMPOSZ2='BLEM POSITION Z2 IN BFEE'                          140
C BLEM X LENGTH                                                           141
      TOFF_BLEMLENX=0.86                                                  141
      CTOFF_BLEMLENX='BLEM X LENGTH'                                      141
C BLEM Y LENGTH                                                           142
      TOFF_BLEMLENY=0.68                                                  142
      CTOFF_BLEMLENY='BLEM Y LENGTH'                                      142
C BLEM Z LENGTH                                                           143
      TOFF_BLEMLENZ=3.80                                                  143
      CTOFF_BLEMLENZ='BLEM Z LENGTH'                                      143
C BPIP POSITION X IN BLEM                                                 144
      TOFF_BPIPPOSX=0.09                                                  144
      CTOFF_BPIPPOSX='BPIP POSITION X IN BLEM'                            144
C BPIP POSITION Y IN BLEM                                                 145
      TOFF_BPIPPOSY=0.00                                                  145
      CTOFF_BPIPPOSY='BPIP POSITION Y IN BLEM'                            145
C BPIP POSISION Z IN BLEM                                                 146
      TOFF_BPIPPOSZ=0.90                                                  146
      CTOFF_BPIPPOSZ='BPIP POSISION Z IN BLEM'                            146
C BPIP RMIN                                                               147
      TOFF_BPIPRMIN=0.31                                                  147
      CTOFF_BPIPRMIN='BPIP RMIN'                                          147
C BPIP RMAX                                                               148
      TOFF_BPIPRMAX=0.34                                                  148
      CTOFF_BPIPRMAX='BPIP RMAX'                                          148
C BPIP Z LENGTH                                                           149
      TOFF_BPIPLENZ=2.00                                                  149
      CTOFF_BPIPLENZ='BPIP Z LENGTH'                                      149
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    150
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           150
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   150
C *                                                                       150
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    150
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           150
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   150
C *                                                                       151
      CALL AGSBEGM(MODULE,IPRIN)                                          151
      IF (IPRIN.LT.0) RETURN                                              151
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       151
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          151
      BPATH_MODR='MODR*'                                                  151
      IDNUM_MODR(1)=0                                                     152
C MODULE HEIGHT (R)                                                       152
      MODR_HEIGHT=1.95                                                    152
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    152
C MODULE WIDTH (PHI)                                                      153
      MODR_WIDTH=21.2                                                     153
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    153
C MODULE LENGTH (Z)                                                       154
      MODR_LENGTH=9.4                                                     154
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    154
C MODULE DETECTOR CENTER IN (PHI)                                         155
      MODR_CENTER=0.35                                                    155
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      155
C MRPC XPOSITIONS                                                         161
      MODR_MRPCX(1)=4.76                                                  161
      MODR_MRPCX(2)=1.21                                                  161
      MODR_MRPCX(3)=4.99                                                  161
      MODR_MRPCX(4)=1.46                                                  161
      MODR_MRPCX(5)=5.19                                                  161
      MODR_MRPCX(6)=1.71                                                  161
      MODR_MRPCX(7)=4.98                                                  161
      MODR_MRPCX(8)=1.54                                                  161
      MODR_MRPCX(9)=2.69                                                  161
      MODR_MRPCX(10)=3.39                                                 161
      MODR_MRPCX(11)=3.25                                                 161
      MODR_MRPCX(12)=3.49                                                 161
      MODR_MRPCX(13)=3.33                                                 161
      MODR_MRPCX(14)=3.54                                                 161
      MODR_MRPCX(15)=3.54                                                 161
      MODR_MRPCX(16)=3.57                                                 161
      MODR_MRPCX(17)=3.48                                                 161
      MODR_MRPCX(18)=3.51                                                 161
      MODR_MRPCX(19)=3.19                                                 161
      MODR_MRPCX(20)=3.19                                                 161
      MODR_MRPCX(21)=3.19                                                 161
      MODR_MRPCX(22)=3.19                                                 161
      MODR_MRPCX(23)=3.19                                                 161
      MODR_MRPCX(24)=3.43                                                 161
      MODR_MRPCX(25)=3.43                                                 161
      MODR_MRPCX(26)=3.43                                                 161
      MODR_MRPCX(27)=3.43                                                 161
      MODR_MRPCX(28)=3.43                                                 161
      MODR_MRPCX(29)=3.43                                                 161
      MODR_MRPCX(30)=3.43                                                 161
      MODR_MRPCX(31)=3.43                                                 161
      MODR_MRPCX(32)=3.43                                                 161
      MODR_MRPCX(33)=3.43                                                 161
      CMODR_MRPCX='MRPC XPOSITIONS'                                       161
C MRPC ZPOSITIONS                                                         168
      MODR_MRPCZ(1)=4.50                                                  168
      MODR_MRPCZ(2)=10.48                                                 168
      MODR_MRPCZ(3)=16.83                                                 168
      MODR_MRPCZ(4)=22.64                                                 168
      MODR_MRPCZ(5)=29.25                                                 168
      MODR_MRPCZ(6)=34.89                                                 168
      MODR_MRPCZ(7)=41.68                                                 168
      MODR_MRPCZ(8)=47.19                                                 168
      MODR_MRPCZ(9)=53.76                                                 168
      MODR_MRPCZ(10)=60.18                                                168
      MODR_MRPCZ(11)=66.53                                                168
      MODR_MRPCZ(12)=72.95                                                168
      MODR_MRPCZ(13)=79.42                                                168
      MODR_MRPCZ(14)=85.97                                                168
      MODR_MRPCZ(15)=92.59                                                168
      MODR_MRPCZ(16)=99.28                                                168
      MODR_MRPCZ(17)=106.00                                               168
      MODR_MRPCZ(18)=112.84                                               168
      MODR_MRPCZ(19)=119.60                                               168
      MODR_MRPCZ(20)=126.66                                               168
      MODR_MRPCZ(21)=133.80                                               168
      MODR_MRPCZ(22)=141.06                                               168
      MODR_MRPCZ(23)=148.43                                               168
      MODR_MRPCZ(24)=156.09                                               168
      MODR_MRPCZ(25)=163.71                                               168
      MODR_MRPCZ(26)=171.46                                               168
      MODR_MRPCZ(27)=179.35                                               168
      MODR_MRPCZ(28)=187.38                                               168
      MODR_MRPCZ(29)=195.56                                               168
      MODR_MRPCZ(30)=203.84                                               168
      MODR_MRPCZ(31)=212.33                                               168
      MODR_MRPCZ(32)=220.94                                               168
      MODR_MRPCZ(33)=229.7                                                168
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       168
C MRPC ANGLES                                                             174
      MODR_MRPCA(1)=1.20                                                  174
      MODR_MRPCA(2)=2.84                                                  174
      MODR_MRPCA(3)=4.48                                                  174
      MODR_MRPCA(4)=6.12                                                  174
      MODR_MRPCA(5)=7.74                                                  174
      MODR_MRPCA(6)=9.36                                                  174
      MODR_MRPCA(7)=10.97                                                 174
      MODR_MRPCA(8)=7.60                                                  174
      MODR_MRPCA(9)=14.30                                                 174
      MODR_MRPCA(10)=22.30                                                174
      MODR_MRPCA(11)=22.30                                                174
      MODR_MRPCA(12)=22.30                                                174
      MODR_MRPCA(13)=22.30                                                174
      MODR_MRPCA(14)=22.30                                                174
      MODR_MRPCA(15)=22.30                                                174
      MODR_MRPCA(16)=22.30                                                174
      MODR_MRPCA(17)=22.30                                                174
      MODR_MRPCA(18)=22.30                                                174
      MODR_MRPCA(19)=33.00                                                174
      MODR_MRPCA(20)=33.00                                                174
      MODR_MRPCA(21)=33.00                                                174
      MODR_MRPCA(22)=33.00                                                174
      MODR_MRPCA(23)=33.00                                                174
      MODR_MRPCA(24)=37.58                                                174
      MODR_MRPCA(25)=37.58                                                174
      MODR_MRPCA(26)=37.58                                                174
      MODR_MRPCA(27)=37.58                                                174
      MODR_MRPCA(28)=37.58                                                174
      MODR_MRPCA(29)=37.58                                                174
      MODR_MRPCA(30)=37.58                                                174
      MODR_MRPCA(31)=37.58                                                174
      MODR_MRPCA(32)=37.58                                                174
      MODR_MRPCA(33)=37.58                                                174
      CMODR_MRPCA='MRPC ANGLES'                                           174
C TRAY INNER SURFACE TO THE CENTER                                        175
      MODR_X0OFFSET=-3.66                                                 175
      CMODR_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   175
C HC->HEIGHT (R)                                                          176
      MODR_HCHGT=0.466                                                    176
      CMODR_HCHGT='HC->HEIGHT (R)'                                        176
C HC->WIDTH (PHI)                                                         177
      MODR_HCWID=20.8                                                     177
      CMODR_HCWID='HC->WIDTH (PHI)'                                       177
C HC->LENGTH (Z)                                                          178
      MODR_HCLEN=8.4                                                      178
      CMODR_HCLEN='HC->LENGTH (Z)'                                        178
C PCB->HEIGHT (R)                                                         179
      MODR_PCBHGT=0.15                                                    179
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      179
C PCB->WIDTH (PHI)                                                        180
      MODR_PCBWID=21.0                                                    180
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     180
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     181
      MODR_PCBLEN=9.4                                                     181
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  181
C MYLAR->HEIGHT                                                           182
      MODR_MYHGT=0.035                                                    182
      CMODR_MYHGT='MYLAR->HEIGHT'                                         182
C MYLAR->WIDTH                                                            183
      MODR_MYWID=21.2                                                     183
      CMODR_MYWID='MYLAR->WIDTH'                                          183
C MYLAR->LENGTH                                                           184
      MODR_MYLEN=8.4                                                      184
      CMODR_MYLEN='MYLAR->LENGTH'                                         184
C GRAPHITE->HEIGHT                                                        185
      MODR_GRHGT=0.013                                                    185
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      185
C GRAPHITE->WIDTH                                                         186
      MODR_GRWID=20.2                                                     186
      CMODR_GRWID='GRAPHITE->WIDTH'                                       186
C GRAPHITE->LENGTH                                                        187
      MODR_GRLEN=7.4                                                      187
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      187
C OUTER GLASS->HEIGHT                                                     188
      MODR_OGHGT=0.11                                                     188
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   188
C OUTER GLASS->WIDTH                                                      189
      MODR_OGWID=20.6                                                     189
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    189
C OUTER GLASS->LENGTH                                                     190
      MODR_OGLEN=7.8                                                      190
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   190
C INNER GLASS->HEIGHT                                                     191
      MODR_IGHGT=0.054                                                    191
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   191
C INNER GLASS->WIDTH                                                      192
      MODR_IGWID=20.0                                                     192
      CMODR_IGWID='INNER GLASS->WIDTH'                                    192
C INNER GLASS->LENGTH                                                     193
      MODR_IGLEN=6.1                                                      193
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   193
C SEPARATOR TUBE->RMIN                                                    194
      MODR_SPRMIN=0.                                                      194
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 194
C SEPARATOR TUBE->RMAX                                                    195
      MODR_SPRMAX=0.011                                                   195
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 195
C SEPARATOR TUBE->LENGTH                                                  196
      MODR_SPLEN=7.8                                                      196
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                196
C WEDGE TUBE: RMIN                                                        197
      MODR_WGRMIN=0.                                                      197
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     197
C WEDGE TUBE->RMAX                                                        198
      MODR_WGRMAX=0.15                                                    198
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     198
C WEDGE TUBE->LENGTH                                                      199
      MODR_WGLEN=10.0                                                     199
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    199
C TOFR FEE PCB THICKNESS                                                  200
      MODR_FEEH=0.15                                                      200
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 200
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    201
      MODR_HBWID=0.635                                                    201
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  201
C NUMBER OF GAPS IN MRPC                                                  202
      MODR_NGAP=6                                                         202
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 202
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    203
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           203
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   203
C *                                                                       203
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    203
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           203
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   203
C *                                                                       204
      CALL AGSBEGM(MODULE,IPRIN)                                          204
      IF (IPRIN.LT.0) RETURN                                              204
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       204
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          204
      BPATH_MOD4='MOD4*'                                                  204
      IDNUM_MOD4(1)=0                                                     205
C MODULE HEIGHT (R)                                                       205
      MOD4_HEIGHT=1.95                                                    205
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    205
C MODULE WIDTH (PHI)                                                      206
      MOD4_WIDTH=21.2                                                     206
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    206
C MODULE LENGTH (Z)                                                       207
      MOD4_LENGTH=9.4                                                     207
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    207
C MODULE DETECTOR CENTER (PHI)                                            208
      MOD4_CENTER=0.35                                                    208
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         208
C MRPC ZPOSNS                                                             215
      MOD4_MRPCZ(1)=5.83                                                  215
      MOD4_MRPCZ(2)=11.97                                                 215
      MOD4_MRPCZ(3)=18.17                                                 215
      MOD4_MRPCZ(4)=24.03                                                 215
      MOD4_MRPCZ(5)=30.55                                                 215
      MOD4_MRPCZ(6)=36.22                                                 215
      MOD4_MRPCZ(7)=42.54                                                 215
      MOD4_MRPCZ(8)=48.87                                                 215
      MOD4_MRPCZ(9)=55.19                                                 215
      MOD4_MRPCZ(10)=61.53                                                215
      MOD4_MRPCZ(11)=67.86                                                215
      MOD4_MRPCZ(12)=74.24                                                215
      MOD4_MRPCZ(13)=80.68                                                215
      MOD4_MRPCZ(14)=87.08                                                215
      MOD4_MRPCZ(15)=93.64                                                215
      MOD4_MRPCZ(16)=100.36                                               215
      MOD4_MRPCZ(17)=107.19                                               215
      MOD4_MRPCZ(18)=114.05                                               215
      MOD4_MRPCZ(19)=121.02                                               215
      MOD4_MRPCZ(20)=128.08                                               215
      MOD4_MRPCZ(21)=135.24                                               215
      MOD4_MRPCZ(22)=142.51                                               215
      MOD4_MRPCZ(23)=149.88                                               215
      MOD4_MRPCZ(24)=157.35                                               215
      MOD4_MRPCZ(25)=164.93                                               215
      MOD4_MRPCZ(26)=172.63                                               215
      MOD4_MRPCZ(27)=180.43                                               215
      MOD4_MRPCZ(28)=188.35                                               215
      MOD4_MRPCZ(29)=196.38                                               215
      MOD4_MRPCZ(30)=204.52                                               215
      MOD4_MRPCZ(31)=212.79                                               215
      MOD4_MRPCZ(32)=221.17                                               215
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           215
C MRPC XPOSNS                                                             221
      MOD4_MRPCX(1)=1.5                                                   221
      MOD4_MRPCX(2)=2.83                                                  221
      MOD4_MRPCX(3)=4.16                                                  221
      MOD4_MRPCX(4)=1.55                                                  221
      MOD4_MRPCX(5)=4.73                                                  221
      MOD4_MRPCX(6)=1.88                                                  221
      MOD4_MRPCX(7)=2.56                                                  221
      MOD4_MRPCX(8)=3.06                                                  221
      MOD4_MRPCX(9)=3.3                                                   221
      MOD4_MRPCX(10)=3.41                                                 221
      MOD4_MRPCX(11)=3.32                                                 221
      MOD4_MRPCX(12)=3.2                                                  221
      MOD4_MRPCX(13)=3.07                                                 221
      MOD4_MRPCX(14)=2.73                                                 221
      MOD4_MRPCX(15)=2.58                                                 221
      MOD4_MRPCX(16)=2.64                                                 221
      MOD4_MRPCX(17)=2.74                                                 221
      MOD4_MRPCX(18)=2.74                                                 221
      MOD4_MRPCX(19)=2.76                                                 221
      MOD4_MRPCX(20)=2.76                                                 221
      MOD4_MRPCX(21)=2.76                                                 221
      MOD4_MRPCX(22)=2.76                                                 221
      MOD4_MRPCX(23)=2.76                                                 221
      MOD4_MRPCX(24)=2.76                                                 221
      MOD4_MRPCX(25)=2.76                                                 221
      MOD4_MRPCX(26)=2.76                                                 221
      MOD4_MRPCX(27)=2.76                                                 221
      MOD4_MRPCX(28)=2.76                                                 221
      MOD4_MRPCX(29)=2.76                                                 221
      MOD4_MRPCX(30)=2.76                                                 221
      MOD4_MRPCX(31)=2.76                                                 221
      MOD4_MRPCX(32)=2.76                                                 221
      CMOD4_MRPCX='MRPC XPOSNS'                                           221
C MRPC ANGLES                                                             225
      MOD4_MRPCA(1)=6.                                                    225
      MOD4_MRPCA(2)=6.                                                    225
      MOD4_MRPCA(3)=6.                                                    225
      MOD4_MRPCA(4)=6.                                                    225
      MOD4_MRPCA(5)=0.                                                    225
      MOD4_MRPCA(6)=12.                                                   225
      MOD4_MRPCA(7)=12.                                                   225
      MOD4_MRPCA(8)=16.                                                   225
      MOD4_MRPCA(9)=16.                                                   225
      MOD4_MRPCA(10)=19.                                                  225
      MOD4_MRPCA(11)=19.                                                  225
      MOD4_MRPCA(12)=19.                                                  225
      MOD4_MRPCA(13)=19.                                                  225
      MOD4_MRPCA(14)=24.                                                  225
      MOD4_MRPCA(15)=24.                                                  225
      MOD4_MRPCA(16)=25.                                                  225
      MOD4_MRPCA(17)=27.                                                  225
      MOD4_MRPCA(18)=29.                                                  225
      MOD4_MRPCA(19)=30.                                                  225
      MOD4_MRPCA(20)=30.                                                  225
      MOD4_MRPCA(21)=30.                                                  225
      MOD4_MRPCA(22)=30.                                                  225
      MOD4_MRPCA(23)=30.                                                  225
      MOD4_MRPCA(24)=30.                                                  225
      MOD4_MRPCA(25)=30.                                                  225
      MOD4_MRPCA(26)=30.                                                  225
      MOD4_MRPCA(27)=30.                                                  225
      MOD4_MRPCA(28)=30.                                                  225
      MOD4_MRPCA(29)=30.                                                  225
      MOD4_MRPCA(30)=30.                                                  225
      MOD4_MRPCA(31)=30.                                                  225
      MOD4_MRPCA(32)=30.                                                  225
      CMOD4_MRPCA='MRPC ANGLES'                                           225
C TRAY INNER SURFACE TO THE CENTER                                        226
      MOD4_X0OFFSET=-3.66                                                 226
      CMOD4_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   226
C HC->HEIGHT (R)                                                          227
      MOD4_HCHGT=0.466                                                    227
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        227
C HC->WIDTH (PHI)                                                         228
      MOD4_HCWID=20.8                                                     228
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       228
C HC->LENGTH (Z)                                                          229
      MOD4_HCLEN=8.4                                                      229
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        229
C PCB->HEIGHT (R)                                                         230
      MOD4_PCBHGT=0.15                                                    230
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      230
C PCB->WIDTH (PHI)                                                        231
      MOD4_PCBWID=21.0                                                    231
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     231
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     232
      MOD4_PCBLEN=9.4                                                     232
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  232
C MYLAR->HEIGHT                                                           233
      MOD4_MYHGT=0.035                                                    233
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         233
C MYLAR->WIDTH                                                            234
      MOD4_MYWID=21.2                                                     234
      CMOD4_MYWID='MYLAR->WIDTH'                                          234
C MYLAR->LENGTH                                                           235
      MOD4_MYLEN=8.4                                                      235
      CMOD4_MYLEN='MYLAR->LENGTH'                                         235
C GRAPHITE->HEIGHT                                                        236
      MOD4_GRHGT=0.013                                                    236
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      236
C GRAPHITE->WIDTH                                                         237
      MOD4_GRWID=20.2                                                     237
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       237
C GRAPHITE->LENGTH                                                        238
      MOD4_GRLEN=7.4                                                      238
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      238
C OUTER GLASS->HEIGHT                                                     239
      MOD4_OGHGT=0.11                                                     239
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   239
C OUTER GLASS->WIDTH                                                      240
      MOD4_OGWID=20.6                                                     240
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    240
C OUTER GLASS->LENGTH                                                     241
      MOD4_OGLEN=7.8                                                      241
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   241
C INNER GLASS->HEIGHT                                                     242
      MOD4_IGHGT=0.054                                                    242
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   242
C INNER GLASS->WIDTH                                                      243
      MOD4_IGWID=20.0                                                     243
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    243
C INNER GLASS->LENGTH                                                     244
      MOD4_IGLEN=6.1                                                      244
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   244
C SEPARATOR TUBE->RMIN                                                    245
      MOD4_SPRMIN=0.                                                      245
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 245
C SEPARATOR TUBE->RMAX                                                    246
      MOD4_SPRMAX=0.011                                                   246
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 246
C SEPARATOR TUBE->LENGTH                                                  247
      MOD4_SPLEN=7.8                                                      247
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                247
C WEDGE TUBE: RMIN                                                        248
      MOD4_WGRMIN=0.                                                      248
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     248
C WEDGE TUBE->RMAX                                                        249
      MOD4_WGRMAX=0.15                                                    249
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     249
C WEDGE TUBE->LENGTH                                                      250
      MOD4_WGLEN=10.0                                                     250
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    250
C TOFR FEE PCB THICKNESS                                                  251
      MOD4_FEEH=0.15                                                      251
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 251
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    252
      MOD4_HBWID=0.635                                                    252
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  252
C NUMBER OF GAPS IN MRPC                                                  253
      MOD4_NGAP=6                                                         253
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 253
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            254
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       254
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      254
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    255
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           255
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   255
C *                                                                       255
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    255
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           255
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   255
C *                                                                       256
      CALL AGSBEGM(MODULE,IPRIN)                                          256
      IF (IPRIN.LT.0) RETURN                                              256
C Filling bank MOD5 - RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS       256
      BANK_TITLE='RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          256
      BPATH_MOD5='MOD5*'                                                  256
      IDNUM_MOD5(1)=0                                                     257
C MODULE HEIGHT (R)                                                       257
      MOD5_HEIGHT=1.95                                                    257
      CMOD5_HEIGHT='MODULE HEIGHT (R)'                                    257
C MODULE WIDTH (PHI)                                                      258
      MOD5_WIDTH=21.2                                                     258
      CMOD5_WIDTH='MODULE WIDTH (PHI)'                                    258
C MODULE LENGTH (Z)                                                       259
      MOD5_LENGTH=9.4                                                     259
      CMOD5_LENGTH='MODULE LENGTH (Z)'                                    259
C MODULE DETECTOR CENTER (PHI)                                            260
      MOD5_CENTER=0.35                                                    260
      CMOD5_CENTER='MODULE DETECTOR CENTER (PHI)'                         260
C MRPC ZPOSNS                                                             267
      MOD5_MRPCZ(1)=7.08                                                  267
      MOD5_MRPCZ(2)=13.23                                                 267
      MOD5_MRPCZ(3)=19.23                                                 267
      MOD5_MRPCZ(4)=25.48                                                 267
      MOD5_MRPCZ(5)=33.27                                                 267
      MOD5_MRPCZ(6)=39.48                                                 267
      MOD5_MRPCZ(7)=45.67                                                 267
      MOD5_MRPCZ(8)=51.51                                                 267
      MOD5_MRPCZ(9)=59.66                                                 267
      MOD5_MRPCZ(10)=65.98                                                267
      MOD5_MRPCZ(11)=71.73                                                267
      MOD5_MRPCZ(12)=78.09                                                267
      MOD5_MRPCZ(13)=86.36                                                267
      MOD5_MRPCZ(14)=92.02                                                267
      MOD5_MRPCZ(15)=98.7                                                 267
      MOD5_MRPCZ(16)=104.28                                               267
      MOD5_MRPCZ(17)=112.45                                               267
      MOD5_MRPCZ(18)=118.79                                               267
      MOD5_MRPCZ(19)=125.14                                               267
      MOD5_MRPCZ(20)=131.51                                               267
      MOD5_MRPCZ(21)=139.6                                                267
      MOD5_MRPCZ(22)=145.96                                               267
      MOD5_MRPCZ(23)=152.32                                               267
      MOD5_MRPCZ(24)=158.68                                               267
      MOD5_MRPCZ(25)=167.41                                               267
      MOD5_MRPCZ(26)=173.78                                               267
      MOD5_MRPCZ(27)=180.18                                               267
      MOD5_MRPCZ(28)=186.6                                                267
      MOD5_MRPCZ(29)=195.53                                               267
      MOD5_MRPCZ(30)=202.01                                               267
      MOD5_MRPCZ(31)=208.52                                               267
      MOD5_MRPCZ(32)=215.06                                               267
      CMOD5_MRPCZ='MRPC ZPOSNS'                                           267
C MRPC XPOSNS                                                             273
      MOD5_MRPCX(1)=1.61                                                  273
      MOD5_MRPCX(2)=4.44                                                  273
      MOD5_MRPCX(3)=1.61                                                  273
      MOD5_MRPCX(4)=5.49                                                  273
      MOD5_MRPCX(5)=2.0                                                   273
      MOD5_MRPCX(6)=3.54                                                  273
      MOD5_MRPCX(7)=5.02                                                  273
      MOD5_MRPCX(8)=2.01                                                  273
      MOD5_MRPCX(9)=3.36                                                  273
      MOD5_MRPCX(10)=5.02                                                 273
      MOD5_MRPCX(11)=2.0                                                  273
      MOD5_MRPCX(12)=3.56                                                 273
      MOD5_MRPCX(13)=5.02                                                 273
      MOD5_MRPCX(14)=2.0                                                  273
      MOD5_MRPCX(15)=5.02                                                 273
      MOD5_MRPCX(16)=2.0                                                  273
      MOD5_MRPCX(17)=2.63                                                 273
      MOD5_MRPCX(18)=3.09                                                 273
      MOD5_MRPCX(19)=3.6                                                  273
      MOD5_MRPCX(20)=4.11                                                 273
      MOD5_MRPCX(21)=3.83                                                 273
      MOD5_MRPCX(22)=4.06                                                 273
      MOD5_MRPCX(23)=4.21                                                 273
      MOD5_MRPCX(24)=4.34                                                 273
      MOD5_MRPCX(25)=3.85                                                 273
      MOD5_MRPCX(26)=3.85                                                 273
      MOD5_MRPCX(27)=3.85                                                 273
      MOD5_MRPCX(28)=3.85                                                 273
      MOD5_MRPCX(29)=3.72                                                 273
      MOD5_MRPCX(30)=3.72                                                 273
      MOD5_MRPCX(31)=3.72                                                 273
      MOD5_MRPCX(32)=3.72                                                 273
      CMOD5_MRPCX='MRPC XPOSNS'                                           273
C MRPC ANGLES                                                             277
      MOD5_MRPCA(1)=0.                                                    277
      MOD5_MRPCA(2)=0.                                                    277
      MOD5_MRPCA(3)=0.                                                    277
      MOD5_MRPCA(4)=0.                                                    277
      MOD5_MRPCA(5)=6.                                                    277
      MOD5_MRPCA(6)=6.                                                    277
      MOD5_MRPCA(7)=6.                                                    277
      MOD5_MRPCA(8)=6.                                                    277
      MOD5_MRPCA(9)=6.                                                    277
      MOD5_MRPCA(10)=6.                                                   277
      MOD5_MRPCA(11)=6.                                                   277
      MOD5_MRPCA(12)=6.                                                   277
      MOD5_MRPCA(13)=6.                                                   277
      MOD5_MRPCA(14)=6.                                                   277
      MOD5_MRPCA(15)=6.                                                   277
      MOD5_MRPCA(16)=6.                                                   277
      MOD5_MRPCA(17)=16.                                                  277
      MOD5_MRPCA(18)=16.                                                  277
      MOD5_MRPCA(19)=16.                                                  277
      MOD5_MRPCA(20)=16.                                                  277
      MOD5_MRPCA(21)=19.                                                  277
      MOD5_MRPCA(22)=19.                                                  277
      MOD5_MRPCA(23)=19.                                                  277
      MOD5_MRPCA(24)=19.                                                  277
      MOD5_MRPCA(25)=22.                                                  277
      MOD5_MRPCA(26)=22.                                                  277
      MOD5_MRPCA(27)=22.                                                  277
      MOD5_MRPCA(28)=22.                                                  277
      MOD5_MRPCA(29)=24.                                                  277
      MOD5_MRPCA(30)=24.                                                  277
      MOD5_MRPCA(31)=24.                                                  277
      MOD5_MRPCA(32)=24.                                                  277
      CMOD5_MRPCA='MRPC ANGLES'                                           277
C TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM JING                  278
      MOD5_X0OFFSET=-3.90                                                 278
      CMOD5_X0OFFSET='TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM   278
     *JING'                                                               278
C HC->HEIGHT (R)                                                          279
      MOD5_HCHGT=0.466                                                    279
      CMOD5_HCHGT='HC->HEIGHT (R)'                                        279
C HC->WIDTH (PHI)                                                         280
      MOD5_HCWID=20.8                                                     280
      CMOD5_HCWID='HC->WIDTH (PHI)'                                       280
C HC->LENGTH (Z)                                                          281
      MOD5_HCLEN=8.4                                                      281
      CMOD5_HCLEN='HC->LENGTH (Z)'                                        281
C PCB->HEIGHT (R)                                                         282
      MOD5_PCBHGT=0.15                                                    282
      CMOD5_PCBHGT='PCB->HEIGHT (R)'                                      282
C PCB->WIDTH (PHI)                                                        283
      MOD5_PCBWID=21.0                                                    283
      CMOD5_PCBWID='PCB->WIDTH (PHI)'                                     283
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     284
      MOD5_PCBLEN=9.4                                                     284
      CMOD5_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  284
C MYLAR->HEIGHT                                                           285
      MOD5_MYHGT=0.035                                                    285
      CMOD5_MYHGT='MYLAR->HEIGHT'                                         285
C MYLAR->WIDTH                                                            286
      MOD5_MYWID=21.2                                                     286
      CMOD5_MYWID='MYLAR->WIDTH'                                          286
C MYLAR->LENGTH                                                           287
      MOD5_MYLEN=8.4                                                      287
      CMOD5_MYLEN='MYLAR->LENGTH'                                         287
C GRAPHITE->HEIGHT                                                        288
      MOD5_GRHGT=0.013                                                    288
      CMOD5_GRHGT='GRAPHITE->HEIGHT'                                      288
C GRAPHITE->WIDTH                                                         289
      MOD5_GRWID=20.2                                                     289
      CMOD5_GRWID='GRAPHITE->WIDTH'                                       289
C GRAPHITE->LENGTH                                                        290
      MOD5_GRLEN=7.4                                                      290
      CMOD5_GRLEN='GRAPHITE->LENGTH'                                      290
C OUTER GLASS->HEIGHT                                                     291
      MOD5_OGHGT=0.11                                                     291
      CMOD5_OGHGT='OUTER GLASS->HEIGHT'                                   291
C OUTER GLASS->WIDTH                                                      292
      MOD5_OGWID=20.6                                                     292
      CMOD5_OGWID='OUTER GLASS->WIDTH'                                    292
C OUTER GLASS->LENGTH                                                     293
      MOD5_OGLEN=7.8                                                      293
      CMOD5_OGLEN='OUTER GLASS->LENGTH'                                   293
C INNER GLASS->HEIGHT                                                     294
      MOD5_IGHGT=0.054                                                    294
      CMOD5_IGHGT='INNER GLASS->HEIGHT'                                   294
C INNER GLASS->WIDTH                                                      295
      MOD5_IGWID=20.0                                                     295
      CMOD5_IGWID='INNER GLASS->WIDTH'                                    295
C INNER GLASS->LENGTH                                                     296
      MOD5_IGLEN=6.1                                                      296
      CMOD5_IGLEN='INNER GLASS->LENGTH'                                   296
C SEPARATOR TUBE->RMIN                                                    297
      MOD5_SPRMIN=0.                                                      297
      CMOD5_SPRMIN='SEPARATOR TUBE->RMIN'                                 297
C SEPARATOR TUBE->RMAX                                                    298
      MOD5_SPRMAX=0.011                                                   298
      CMOD5_SPRMAX='SEPARATOR TUBE->RMAX'                                 298
C SEPARATOR TUBE->LENGTH                                                  299
      MOD5_SPLEN=7.8                                                      299
      CMOD5_SPLEN='SEPARATOR TUBE->LENGTH'                                299
C WEDGE TUBE: RMIN                                                        300
      MOD5_WGRMIN=0.                                                      300
      CMOD5_WGRMIN='WEDGE TUBE: RMIN'                                     300
C WEDGE TUBE->RMAX                                                        301
      MOD5_WGRMAX=0.15                                                    301
      CMOD5_WGRMAX='WEDGE TUBE->RMAX'                                     301
C WEDGE TUBE->LENGTH                                                      302
      MOD5_WGLEN=10.0                                                     302
      CMOD5_WGLEN='WEDGE TUBE->LENGTH'                                    302
C TOFR FEE PCB THICKNESS                                                  303
      MOD5_FEEH=0.15                                                      303
      CMOD5_FEEH='TOFR FEE PCB THICKNESS'                                 303
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    304
      MOD5_HBWID=0.635                                                    304
      CMOD5_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  304
C NUMBER OF GAPS IN MRPC                                                  305
      MOD5_NGAP=6                                                         305
      CMOD5_NGAP='NUMBER OF GAPS IN MRPC'                                 305
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ PUTS THIS NEW VALUE   306
      MOD5_TRAYEDGEZ=13.25                                                306
      CMOD5_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ  306
     * PUTS THIS NEW VALUE'                                               306
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    307
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           307
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   307
C *                                                                       307
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    307
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           307
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   307
C *                                                                       308
      CALL AGSBEGM(MODULE,IPRIN)                                          308
      IF (IPRIN.LT.0) RETURN                                              308
C Filling bank MOD7 - RUN7 MRPC TOF MODULE DIMENSIONS AND POSITIONS       308
      BANK_TITLE='RUN7 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          308
      BPATH_MOD7='MOD7*'                                                  308
      IDNUM_MOD7(1)=0                                                     309
C MODULE HEIGHT (R)                                                       309
      MOD7_HEIGHT=1.95                                                    309
      CMOD7_HEIGHT='MODULE HEIGHT (R)'                                    309
C MODULE WIDTH (PHI)                                                      310
      MOD7_WIDTH=21.2                                                     310
      CMOD7_WIDTH='MODULE WIDTH (PHI)'                                    310
C MODULE LENGTH (Z)                                                       311
      MOD7_LENGTH=9.4                                                     311
      CMOD7_LENGTH='MODULE LENGTH (Z)'                                    311
C MODULE DETECTOR CENTER (PHI)                                            312
      MOD7_CENTER=0.35                                                    312
      CMOD7_CENTER='MODULE DETECTOR CENTER (PHI)'                         312
C MRPC ZPOSNS                                                             319
      MOD7_MRPCZ(1)=5.43                                                  319
      MOD7_MRPCZ(2)=11.42                                                 319
      MOD7_MRPCZ(3)=17.71                                                 319
      MOD7_MRPCZ(4)=23.56                                                 319
      MOD7_MRPCZ(5)=29.96                                                 319
      MOD7_MRPCZ(6)=35.66                                                 319
      MOD7_MRPCZ(7)=42.71                                                 319
      MOD7_MRPCZ(8)=49.04                                                 319
      MOD7_MRPCZ(9)=55.35                                                 319
      MOD7_MRPCZ(10)=61.65                                                319
      MOD7_MRPCZ(11)=67.99                                                319
      MOD7_MRPCZ(12)=74.42                                                319
      MOD7_MRPCZ(13)=80.91                                                319
      MOD7_MRPCZ(14)=87.44                                                319
      MOD7_MRPCZ(15)=93.93                                                319
      MOD7_MRPCZ(16)=100.53                                               319
      MOD7_MRPCZ(17)=107.30                                               319
      MOD7_MRPCZ(18)=114.17                                               319
      MOD7_MRPCZ(19)=121.22                                               319
      MOD7_MRPCZ(20)=128.29                                               319
      MOD7_MRPCZ(21)=135.52                                               319
      MOD7_MRPCZ(22)=142.80                                               319
      MOD7_MRPCZ(23)=150.19                                               319
      MOD7_MRPCZ(24)=157.72                                               319
      MOD7_MRPCZ(25)=165.34                                               319
      MOD7_MRPCZ(26)=173.07                                               319
      MOD7_MRPCZ(27)=180.92                                               319
      MOD7_MRPCZ(28)=188.93                                               319
      MOD7_MRPCZ(29)=197.02                                               319
      MOD7_MRPCZ(30)=205.24                                               319
      MOD7_MRPCZ(31)=213.61                                               319
      MOD7_MRPCZ(32)=222.08                                               319
      CMOD7_MRPCZ='MRPC ZPOSNS'                                           319
C MRPC XPOSNS                                                             325
      MOD7_MRPCX(1)=3.94                                                  325
      MOD7_MRPCX(2)=1.48                                                  325
      MOD7_MRPCX(3)=3.94                                                  325
      MOD7_MRPCX(4)=1.48                                                  325
      MOD7_MRPCX(5)=3.94                                                  325
      MOD7_MRPCX(6)=1.48                                                  325
      MOD7_MRPCX(7)=3.01                                                  325
      MOD7_MRPCX(8)=3.54                                                  325
      MOD7_MRPCX(9)=3.79                                                  325
      MOD7_MRPCX(10)=3.86                                                 325
      MOD7_MRPCX(11)=3.84                                                 325
      MOD7_MRPCX(12)=3.89                                                 325
      MOD7_MRPCX(13)=3.89                                                 325
      MOD7_MRPCX(14)=3.84                                                 325
      MOD7_MRPCX(15)=3.51                                                 325
      MOD7_MRPCX(16)=3.29                                                 325
      MOD7_MRPCX(17)=3.29                                                 325
      MOD7_MRPCX(18)=3.29                                                 325
      MOD7_MRPCX(19)=3.50                                                 325
      MOD7_MRPCX(20)=3.50                                                 325
      MOD7_MRPCX(21)=3.60                                                 325
      MOD7_MRPCX(22)=3.60                                                 325
      MOD7_MRPCX(23)=3.60                                                 325
      MOD7_MRPCX(24)=3.60                                                 325
      MOD7_MRPCX(25)=3.60                                                 325
      MOD7_MRPCX(26)=3.60                                                 325
      MOD7_MRPCX(27)=3.60                                                 325
      MOD7_MRPCX(28)=3.60                                                 325
      MOD7_MRPCX(29)=3.60                                                 325
      MOD7_MRPCX(30)=3.60                                                 325
      MOD7_MRPCX(31)=3.60                                                 325
      MOD7_MRPCX(32)=3.60                                                 325
      CMOD7_MRPCX='MRPC XPOSNS'                                           325
C MRPC ANGLES                                                             329
      MOD7_MRPCA(1)=0.                                                    329
      MOD7_MRPCA(2)=0.                                                    329
      MOD7_MRPCA(3)=0.                                                    329
      MOD7_MRPCA(4)=0.                                                    329
      MOD7_MRPCA(5)=0.                                                    329
      MOD7_MRPCA(6)=0.                                                    329
      MOD7_MRPCA(7)=16.                                                   329
      MOD7_MRPCA(8)=16.                                                   329
      MOD7_MRPCA(9)=20.                                                   329
      MOD7_MRPCA(10)=22.                                                  329
      MOD7_MRPCA(11)=22.                                                  329
      MOD7_MRPCA(12)=22.                                                  329
      MOD7_MRPCA(13)=22.                                                  329
      MOD7_MRPCA(14)=22.                                                  329
      MOD7_MRPCA(15)=26.                                                  329
      MOD7_MRPCA(16)=26.                                                  329
      MOD7_MRPCA(17)=26.                                                  329
      MOD7_MRPCA(18)=26.                                                  329
      MOD7_MRPCA(19)=30.                                                  329
      MOD7_MRPCA(20)=30.                                                  329
      MOD7_MRPCA(21)=32.                                                  329
      MOD7_MRPCA(22)=32.                                                  329
      MOD7_MRPCA(23)=32.                                                  329
      MOD7_MRPCA(24)=32.                                                  329
      MOD7_MRPCA(25)=32.                                                  329
      MOD7_MRPCA(26)=32.                                                  329
      MOD7_MRPCA(27)=32.                                                  329
      MOD7_MRPCA(28)=32.                                                  329
      MOD7_MRPCA(29)=32.                                                  329
      MOD7_MRPCA(30)=32.                                                  329
      MOD7_MRPCA(31)=32.                                                  329
      MOD7_MRPCA(32)=32.                                                  329
      CMOD7_MRPCA='MRPC ANGLES'                                           329
C TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TRAY_WALLTHK            330
      MOD7_X0OFFSET=-4.32                                                 330
      CMOD7_X0OFFSET='TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TR  330
     *AY_WALLTHK'                                                         330
C HC->HEIGHT (R)                                                          331
      MOD7_HCHGT=0.466                                                    331
      CMOD7_HCHGT='HC->HEIGHT (R)'                                        331
C HC->WIDTH (PHI)                                                         332
      MOD7_HCWID=20.8                                                     332
      CMOD7_HCWID='HC->WIDTH (PHI)'                                       332
C HC->LENGTH (Z)                                                          333
      MOD7_HCLEN=8.4                                                      333
      CMOD7_HCLEN='HC->LENGTH (Z)'                                        333
C PCB->HEIGHT (R)                                                         334
      MOD7_PCBHGT=0.15                                                    334
      CMOD7_PCBHGT='PCB->HEIGHT (R)'                                      334
C PCB->WIDTH (PHI)                                                        335
      MOD7_PCBWID=21.0                                                    335
      CMOD7_PCBWID='PCB->WIDTH (PHI)'                                     335
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     336
      MOD7_PCBLEN=9.4                                                     336
      CMOD7_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  336
C MYLAR->HEIGHT                                                           337
      MOD7_MYHGT=0.035                                                    337
      CMOD7_MYHGT='MYLAR->HEIGHT'                                         337
C MYLAR->WIDTH                                                            338
      MOD7_MYWID=21.2                                                     338
      CMOD7_MYWID='MYLAR->WIDTH'                                          338
C MYLAR->LENGTH                                                           339
      MOD7_MYLEN=8.4                                                      339
      CMOD7_MYLEN='MYLAR->LENGTH'                                         339
C GRAPHITE->HEIGHT                                                        340
      MOD7_GRHGT=0.013                                                    340
      CMOD7_GRHGT='GRAPHITE->HEIGHT'                                      340
C GRAPHITE->WIDTH                                                         341
      MOD7_GRWID=20.2                                                     341
      CMOD7_GRWID='GRAPHITE->WIDTH'                                       341
C GRAPHITE->LENGTH                                                        342
      MOD7_GRLEN=7.4                                                      342
      CMOD7_GRLEN='GRAPHITE->LENGTH'                                      342
C OUTER GLASS->HEIGHT                                                     343
      MOD7_OGHGT=0.11                                                     343
      CMOD7_OGHGT='OUTER GLASS->HEIGHT'                                   343
C OUTER GLASS->WIDTH                                                      344
      MOD7_OGWID=20.6                                                     344
      CMOD7_OGWID='OUTER GLASS->WIDTH'                                    344
C OUTER GLASS->LENGTH                                                     345
      MOD7_OGLEN=7.8                                                      345
      CMOD7_OGLEN='OUTER GLASS->LENGTH'                                   345
C INNER GLASS->HEIGHT                                                     346
      MOD7_IGHGT=0.054                                                    346
      CMOD7_IGHGT='INNER GLASS->HEIGHT'                                   346
C INNER GLASS->WIDTH                                                      347
      MOD7_IGWID=20.0                                                     347
      CMOD7_IGWID='INNER GLASS->WIDTH'                                    347
C INNER GLASS->LENGTH                                                     348
      MOD7_IGLEN=6.1                                                      348
      CMOD7_IGLEN='INNER GLASS->LENGTH'                                   348
C SEPARATOR TUBE->RMIN                                                    349
      MOD7_SPRMIN=0.                                                      349
      CMOD7_SPRMIN='SEPARATOR TUBE->RMIN'                                 349
C SEPARATOR TUBE->RMAX                                                    350
      MOD7_SPRMAX=0.011                                                   350
      CMOD7_SPRMAX='SEPARATOR TUBE->RMAX'                                 350
C SEPARATOR TUBE->LENGTH                                                  351
      MOD7_SPLEN=7.8                                                      351
      CMOD7_SPLEN='SEPARATOR TUBE->LENGTH'                                351
C WEDGE TUBE: RMIN                                                        352
      MOD7_WGRMIN=0.                                                      352
      CMOD7_WGRMIN='WEDGE TUBE: RMIN'                                     352
C WEDGE TUBE->RMAX                                                        353
      MOD7_WGRMAX=0.15                                                    353
      CMOD7_WGRMAX='WEDGE TUBE->RMAX'                                     353
C WEDGE TUBE->LENGTH                                                      354
      MOD7_WGLEN=10.0                                                     354
      CMOD7_WGLEN='WEDGE TUBE->LENGTH'                                    354
C TOFR FEE PCB THICKNESS                                                  355
      MOD7_FEEH=0.15                                                      355
      CMOD7_FEEH='TOFR FEE PCB THICKNESS'                                 355
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    356
      MOD7_HBWID=0.635                                                    356
      CMOD7_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  356
C NUMBER OF GAPS IN MRPC                                                  357
      MOD7_NGAP=6                                                         357
      CMOD7_NGAP='NUMBER OF GAPS IN MRPC'                                 357
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE                                358
      MOD7_TRAYEDGEZ=0.50                                                 358
      CMOD7_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE'          358
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    359
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           359
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   359
C *                                                                       359
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    359
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           359
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   359
C use default BTOG bank                                                   360
      BPATH_BTOG='BTOG*'                                                  360
      IDNUM_BTOG(1)=0                                                     360
      CALL AGDGETP('BTOFGEO5','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  360
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      360
     *LINK_BTOG,FLAG_BTOG)                                                360
C use default TRAY bank                                                   361
      BPATH_TRAY='TRAY*'                                                  361
      IDNUM_TRAY(1)=0                                                     361
      CALL AGDGETP('BTOFGEO5','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  361
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      361
     *LINK_TRAY,FLAG_TRAY)                                                361
C use default CTBB bank                                                   362
      BPATH_CTBB='CTBB*'                                                  362
      IDNUM_CTBB(1)=0                                                     362
      CALL AGDGETP('BTOFGEO5','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  362
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      362
     *LINK_CTBB,FLAG_CTBB)                                                362
C use default TOFF bank                                                   363
      BPATH_TOFF='TOFF*'                                                  363
      IDNUM_TOFF(1)=0                                                     363
      CALL AGDGETP('BTOFGEO5','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  363
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      363
     *LINK_TOFF,FLAG_TOFF)                                                363
C use default MODR bank                                                   364
      BPATH_MODR='MODR*'                                                  364
      IDNUM_MODR(1)=0                                                     364
      CALL AGDGETP('BTOFGEO5','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  364
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      364
     *LINK_MODR,FLAG_MODR)                                                364
C use default MOD4 bank                                                   365
      BPATH_MOD4='MOD4*'                                                  365
      IDNUM_MOD4(1)=0                                                     365
      CALL AGDGETP('BTOFGEO5','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  365
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      365
     *LINK_MOD4,FLAG_MOD4)                                                365
C use default MOD5 bank                                                   366
      BPATH_MOD5='MOD5*'                                                  366
      IDNUM_MOD5(1)=0                                                     366
      CALL AGDGETP('BTOFGEO5','*',-999,'MOD5',' ',BANK_MOD5, BPATH_MOD5,  366
     *IDNUM_MOD5,1,MAP_MOD5,NAMES_MOD5,BANK_MOD5,LEN_MOD5,LLEN_MOD5,      366
     *LINK_MOD5,FLAG_MOD5)                                                366
C use default MOD7 bank                                                   367
      BPATH_MOD7='MOD7*'                                                  367
      IDNUM_MOD7(1)=0                                                     367
      CALL AGDGETP('BTOFGEO5','*',-999,'MOD7',' ',BANK_MOD7, BPATH_MOD7,  367
     *IDNUM_MOD7,1,MAP_MOD7,NAMES_MOD7,BANK_MOD7,LEN_MOD7,LLEN_MOD7,      367
     *LINK_MOD7,FLAG_MOD7)                                                367
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             368
      CALL AGSRESET                                                       368
      AG_A = 28.08                                                        368
      AG_Z = 14                                                           368
      AG_W = 0.6*1*28./60.                                                368
      AG_TITLE = 'SI'                                                     368
      AG_EXNAME = 'COMPONENT'                                             368
      AG_PARLIST = 'A_Z_W'                                                368
      CALL AXCOMPONENT                                                    368
C *                                                                       368
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  368
C =                                                                       368
      CALL AGSRESET                                                       369
      AG_A = 16                                                           369
      AG_Z = 8                                                            369
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               369
      AG_TITLE = 'O'                                                      369
      AG_EXNAME = 'COMPONENT'                                             369
      AG_PARLIST = 'A_Z_W'                                                369
      CALL AXCOMPONENT                                                    369
C *                                                                       369
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 369
      CALL AGSRESET                                                       370
      AG_A = 12                                                           370
      AG_Z = 6                                                            370
      AG_W = 0.4*8*12./174.                                               370
      AG_TITLE = 'C'                                                      370
      AG_EXNAME = 'COMPONENT'                                             370
      AG_PARLIST = 'A_Z_W'                                                370
      CALL AXCOMPONENT                                                    370
C *                                                                       370
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  370
      CALL AGSRESET                                                       371
      AG_A = 1                                                            371
      AG_Z = 1                                                            371
      AG_W = 0.4*14*1./174.                                               371
      AG_TITLE = 'H'                                                      371
      AG_EXNAME = 'COMPONENT'                                             371
      AG_PARLIST = 'A_Z_W'                                                371
      CALL AXCOMPONENT                                                    371
C *                                                                       371
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  371
      CALL AGSRESET                                                       372
      AG_DENS = 1.7                                                       372
      AG_TITLE = 'G10'                                                    372
      AG_EXNAME = 'MIXTURE'                                               372
      AG_PARLIST = 'DENS'                                                 372
      CALL AXMIXTURE                                                      372
C *                                                                       372
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  372
C ==                                                                      372
      CALL AGSRESET                                                       373
      AG_A = 1                                                            373
      AG_Z = 1                                                            373
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             373
      AG_TITLE = 'H'                                                      373
      AG_EXNAME = 'COMPONENT'                                             373
      AG_PARLIST = 'A_Z_W'                                                373
      CALL AXCOMPONENT                                                    373
C *                                                                       373
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   373
C <===                                                                    373
      CALL AGSRESET                                                       374
      AG_A = 12                                                           374
      AG_Z = 6                                                            374
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            374
      AG_TITLE = 'C'                                                      374
      AG_EXNAME = 'COMPONENT'                                             374
      AG_PARLIST = 'A_Z_W'                                                374
      CALL AXCOMPONENT                                                    374
C *                                                                       374
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  374
C  <===                                                                   374
      CALL AGSRESET                                                       375
      AG_A = 19                                                           375
      AG_Z = 9                                                            375
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           375
      AG_TITLE = 'F'                                                      375
      AG_EXNAME = 'COMPONENT'                                             375
      AG_PARLIST = 'A_Z_W'                                                375
      CALL AXCOMPONENT                                                    375
C *                                                                       375
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         375
      CALL AGSRESET                                                       376
      AG_A = 32                                                           376
      AG_Z = 16                                                           376
      AG_W = 0.+0.05*1*32./146.+0.                                        376
      AG_TITLE = 'S'                                                      376
      AG_EXNAME = 'COMPONENT'                                             376
      AG_PARLIST = 'A_Z_W'                                                376
      CALL AXCOMPONENT                                                    376
C *                                                                       376
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           376
      CALL AGSRESET                                                       377
      AG_DENS = 4.55E-3                                                   377
      AG_TITLE = 'RPCGAS'                                                 377
      AG_EXNAME = 'MIXTURE'                                               377
      AG_PARLIST = 'DENS'                                                 377
      CALL AXMIXTURE                                                      377
C *                                                                       377
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       377
      CALL AGSRESET                                                       378
      AG_A = 27                                                           378
      AG_Z = 13                                                           378
      AG_W = 0.0105                                                       378
      AG_TITLE = 'AL'                                                     378
      AG_EXNAME = 'COMPONENT'                                             378
      AG_PARLIST = 'A_Z_W'                                                378
      CALL AXCOMPONENT                                                    378
C *                                                                       378
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         378
      CALL AGSRESET                                                       379
      AG_A = 14                                                           379
      AG_Z = 7                                                            379
      AG_W = 0.7395                                                       379
      AG_TITLE = 'N'                                                      379
      AG_EXNAME = 'COMPONENT'                                             379
      AG_PARLIST = 'A_Z_W'                                                379
      CALL AXCOMPONENT                                                    379
C *                                                                       379
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 379
      CALL AGSRESET                                                       380
      AG_A = 9                                                            380
      AG_Z = 4.5                                                          380
      AG_W = 0.2500                                                       380
      AG_TITLE = 'ADHESIVE'                                               380
      AG_EXNAME = 'COMPONENT'                                             380
      AG_PARLIST = 'A_Z_W'                                                380
      CALL AXCOMPONENT                                                    380
C *                                                                       380
C Geant MIXTURE : HONEYCOMB ===> DENS=0.282 <===                          380
      CALL AGSRESET                                                       381
      AG_DENS = 0.282                                                     381
      AG_TITLE = 'HONEYCOMB'                                              381
      AG_EXNAME = 'MIXTURE'                                               381
      AG_PARLIST = 'DENS'                                                 381
      CALL AXMIXTURE                                                      381
C *                                                                       381
      CALL AGSBEGM(MODULE,IPRIN)                                          381
C Check IPRIN<0                                                           382
      IF (IPRIN.LT.0) RETURN                                              382
C Create BTOF :                                                           382
C Call To Local Procedure BTOF                                            382
      ASSIGN 5020 TO BLOCK_BTOF                                           382
      GO TO  5010                                                         382
5020  CONTINUE                                                            382
C *                                                                       382
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           383
      CALL AGSRESET                                                       383
      AG_MOTHER = 'CAVE'                                                  383
      AG_TITLE = 'BTOF'                                                   383
      AG_EXNAME = 'POSITION'                                              383
      AG_PARLIST = 'MOTHER'                                               383
      CALL AXPOSITION                                                     383
C *                                                                       383
C ----------------------------------------------------------------------  383
C ------------ BTOF                                                       383
      CALL AGSENDM                                                        385
      RETURN                                                              385
C Begin Local Procedure BTOF                                              385
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   385
5010  CONTINUE                                                            385
         AG_TITLE='BTOF'                                                  385
         AG_MODULE=MODULE                                                 385
         AG_MARK='V00'                                                    385
         CALL AGSPUSH                                                     385
         AG_MODULE=MODULE                                                 385
C    *                                                                    385
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       386
         CALL AGSRESET                                                    386
         AG_SEEN = 1                                                      386
         AG_COLO = 1                                                      386
         AG_TITLE = 'BTOF'                                                386
         AG_EXNAME = 'ATTRIBUTE'                                          386
         AG_PARLIST = 'SEEN_COLO'                                         386
         CALL AXATTRIBUTE                                                 386
C    *                                                                    386
C    Geant MATERIAL : AIR ===> inherited arguments used <===              386
         CALL AGSRESET                                                    387
         AG_TITLE = 'AIR'                                                 387
         AG_EXNAME = 'MATERIAL'                                           387
         AG_PARLIST = 'NONE'                                              387
         CALL AXMATERIAL                                                  387
C    *                                                                    387
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           387
         CALL AGSRESET                                                    388
         AG_TITLE = 'STANDARD'                                            388
         AG_EXNAME = 'MEDIUM'                                             388
         AG_PARLIST = 'NONE'                                              388
         CALL AXMEDIUM                                                    388
C    *                                                                    388
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN RMAX=BTOG_RMAX, DZ=BTOG_DZ <  388
C    ===                                                                  388
         CALL AGSRESET                                                    390
         AG_RMIN = BTOG_RMIN                                              390
         AG_RMAX = BTOG_RMAX                                              390
         AG_DZ = BTOG_DZ                                                  390
         AG_TITLE = 'TUBE'                                                390
         AG_EXNAME = 'SHAPE'                                              390
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      390
         CALL AXSHAPE                                                     390
C    *                                                                    390
         WRITE(LOUT,5030)BTOG_CHOICE,5                                    390
5030     FORMAT('BTOF: btog_choice = ', F4.1, ' itof = ', I2)             392
         YEAR = BTOG_CHOICE - 3                                           393
         WRITE(LOUT,5040)YEAR                                             393
5040     FORMAT(' : this is the run ',I2,' geometry ...')                 395
C    CTB                                                                  396
         CHOICE = 1                                                       396
C    Check (BTOG_CHOICE==2)                                               397
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     397
C    FULL TOFP                                                            398
         CHOICE=BTOG_CHOICE                                               398
         ENDIF                                                            399
C    Check (BTOG_CHOICE==6)                                               400
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     400
C    FULL TOFR                                                            401
         CHOICE=BTOG_CHOICE                                               401
         ENDIF                                                            402
C    Check (BTOG_CHOICE==11)                                              403
         IF ((BTOG_CHOICE.EQ.11)) THEN                                    403
C    RUN VIII, 2 TRAYS IN WEST                                            404
         CHOICE=BTOG_CHOICE+1                                             404
         ENDIF                                                            405
C    Create BTOH :                                                        406
C    Call To Local Procedure BTOH                                         406
         ASSIGN 5060 TO BLOCK_BTOH                                        406
         GO TO  5050                                                      406
5060     CONTINUE                                                         406
C    *                                                                    406
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2 ALPHAY=180 <===              407
         CALL AGSRESET                                                    407
         AG_Z = +BTOG_DZ/2                                                407
         AG_ALPHAN=AG_ALPHAN+1                                            407
         AG_ALPHAK(AG_ALPHAN)=2                                           407
         AG_ALPHAL(AG_ALPHAN)=180                                         407
         AG_TITLE = 'BTOH'                                                407
         AG_EXNAME = 'POSITION'                                           407
         AG_PARLIST = 'Z_ALPHAY'                                          407
         CALL AXPOSITION                                                  407
C    *                                                                    407
         CHOICE=BTOG_CHOICE                                               407
C    Create BTOH :                                                        409
C    Call To Local Procedure BTOH                                         409
         ASSIGN 5070 TO BLOCK_BTOH                                        409
         GO TO  5050                                                      409
5070     CONTINUE                                                         409
C    *                                                                    409
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2 <===                         410
         CALL AGSRESET                                                    410
         AG_Z = -BTOG_DZ/2                                                410
         AG_TITLE = 'BTOH'                                                410
         AG_EXNAME = 'POSITION'                                           410
         AG_PARLIST = 'Z'                                                 410
         CALL AXPOSITION                                                  410
C    *                                                                    410
5011  CALL AGSPOPB                                                        411
      GO TO BLOCK_BTOF, (5010 ,5020)                                      411
C End of Local Procedure                                                  411
C ----------------------------------------------------------------------  412
C ------------ BTOH                                                       412
C Begin Local Procedure BTOH                                              413
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            413
5050  CONTINUE                                                            413
         AG_TITLE='BTOH'                                                  413
         AG_MODULE=MODULE                                                 413
         AG_MARK='V00'                                                    413
         CALL AGSPUSH                                                     413
         AG_MODULE=MODULE                                                 413
C    *                                                                    413
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         414
         CALL AGSRESET                                                    414
         AG_SEEN = 0                                                      414
         AG_COLO = 1                                                      414
         AG_SERIAL = CHOICE                                               414
         AG_TITLE = 'BTOH'                                                414
         AG_EXNAME = 'ATTRIBUTE'                                          414
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  414
         CALL AXATTRIBUTE                                                 414
C    *                                                                    414
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            414
         CALL AGSRESET                                                    415
         AG_DZ = BTOG_DZ/2                                                415
         AG_TITLE = 'TUBE'                                                415
         AG_EXNAME = 'SHAPE'                                              415
         AG_PARLIST = 'DZ'                                                415
         CALL AXSHAPE                                                     415
C    *                                                                    415
C    *                                                                    415
         IS=1                                                             416
            GO TO 5083                                                    416
5081        IS=IS+1                                                       416
5083        IF(IS-(60).GT.0) GO TO 5082                                   416
C       // ALL CTB FOR CHOICE=1                                           417
            TOF=0                                                         417
C       Check (CHOICE==2)                                                 418
            IF ((CHOICE.EQ.2)) THEN                                       418
C       // ALL TOFP                                                       419
            TOF=1                                                         419
            ENDIF                                                         420
C       Check (CHOICE==3&46<=IS&IS<=60)                                   421
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             421
C       // BIG TOFP PATCH, REST CTB                                       422
            TOF=1                                                         422
            ENDIF                                                         423
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              424
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              424
C       // RUN-2 (ONE TOFP TRAY)                                          425
            TOF=1                                                         425
            ENDIF                                                         426
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              427
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              427
C       RUN3ONE TOFP TRAY                                                 428
            TOF=1                                                         428
            ENDIF                                                         429
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 430
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 430
C       AND ONE TOFR TRAY                                                 431
            TOF=2                                                         431
            ENDIF                                                         432
C       Check (CHOICE==6)                                                 433
            IF ((CHOICE.EQ.6)) THEN                                       433
C       // ALL TOFR                                                       434
            TOF=2                                                         434
            ENDIF                                                         435
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              436
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              436
C       RUN4ONE TOFP TRAY MOVED 1 SLOT                                    437
            TOF=1                                                         437
            ENDIF                                                         438
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 439
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 439
C       AND ONE TOFRP TRAY                                                440
            TOF=3                                                         440
            ENDIF                                                         441
C       Check (CHOICE==8&IS==BTOG_POSIT3)                                 442
            IF ((CHOICE.EQ.8.AND.IS.EQ.BTOG_POSIT3)) THEN                 442
C       // RUN-5 (ONE TOFR5 TRAY)                                         443
            TOF=4                                                         443
            ENDIF                                                         444
C       Check (CHOICE==9&IS==BTOG_POSIT3)                                 445
            IF ((CHOICE.EQ.9.AND.IS.EQ.BTOG_POSIT3)) THEN                 445
C       // RUN-6 (ONE TOFR6 TRAY)                                         446
            TOF=5                                                         446
            ENDIF                                                         447
C       Check (CHOICE==10&IS==BTOG_POSIT3)                                448
            IF ((CHOICE.EQ.10.AND.IS.EQ.BTOG_POSIT3)) THEN                448
C       // RUN-7 (ONE TOFR7 TRAY)                                         449
            TOF=6                                                         449
            ENDIF                                                         450
C       Check (CHOICE==11&IS==BTOG_POSIT4(1))                             451
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(1))) THEN             451
C       // RUN-8 (5 TOFR8 TRAYS)                                          452
            TOF=7                                                         452
            ENDIF                                                         453
C       Check (CHOICE==11&IS==BTOG_POSIT4(2))                             454
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(2))) THEN             454
C       // RUN-8 (5 TOFR8 TRAYS)                                          455
            TOF=7                                                         455
            ENDIF                                                         456
C       Check (CHOICE==11&IS==BTOG_POSIT4(3))                             457
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(3))) THEN             457
C       // RUN-8 (5 TOFR8 TRAYS)                                          458
            TOF=7                                                         458
            ENDIF                                                         459
C       Check (CHOICE==12&IS==BTOG_POSIT4(4))                             460
            IF ((CHOICE.EQ.12.AND.IS.EQ.BTOG_POSIT4(4))) THEN             460
C       // RUN-8 (5 TOFR8 TRAYS)                                          461
            TOF=7                                                         461
            ENDIF                                                         462
C       Check (CHOICE==12&IS==BTOG_POSIT4(5))                             463
            IF ((CHOICE.EQ.12.AND.IS.EQ.BTOG_POSIT4(5))) THEN             463
C       // RUN-8 (5 TOFR8 TRAYS)                                          464
            TOF=7                                                         464
            ENDIF                                                         465
C       Create BSEC :                                                     466
C       Call To Local Procedure BSEC                                      466
            ASSIGN 5100 TO BLOCK_BSEC                                     466
            GO TO  5090                                                   466
5100        CONTINUE                                                      466
C       *                                                                 466
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   467
            CALL AGSRESET                                                 467
            AG_ALPHAN=AG_ALPHAN+1                                         467
            AG_ALPHAK(AG_ALPHAN)=3                                        467
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 467
            AG_TITLE = 'BSEC'                                             467
            AG_EXNAME = 'POSITION'                                        467
            AG_PARLIST = 'ALPHAZ'                                         467
            CALL AXPOSITION                                               467
C       *                                                                 467
         GO TO 5081                                                       468
5082     CONTINUE                                                         468
5051  CALL AGSPOPB                                                        469
      GO TO BLOCK_BTOH, (5050 ,5060,5070)                                 469
C End of Local Procedure                                                  469
C ----------------------------------------------------------------------  470
C ------------ BSEC                                                       470
C Begin Local Procedure BSEC                                              471
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                471
5090  CONTINUE                                                            471
         AG_TITLE='BSEC'                                                  471
         AG_MODULE=MODULE                                                 471
         AG_MARK='V00'                                                    471
         CALL AGSPUSH                                                     471
         AG_MODULE=MODULE                                                 471
C    *                                                                    471
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            472
         CALL AGSRESET                                                    472
         AG_SEEN = 0                                                      472
         AG_COLO = 1                                                      472
         AG_SERIAL = TOF                                                  472
         AG_TITLE = 'BSEC'                                                472
         AG_EXNAME = 'ATTRIBUTE'                                          472
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  472
         CALL AXATTRIBUTE                                                 472
C    *                                                                    472
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      472
         CALL AGSRESET                                                    473
         AG_PHI1 = -3.0                                                   473
         AG_PHI2 = 3.0                                                    473
         AG_TITLE = 'TUBS'                                                473
         AG_EXNAME = 'SHAPE'                                              473
         AG_PARLIST = 'PHI1_PHI2'                                         473
         CALL AXSHAPE                                                     473
C    *                                                                    473
C    Create BTRA :                                                        474
C    Call To Local Procedure BTRA                                         474
         ASSIGN 5120 TO BLOCK_BTRA                                        474
         GO TO  5110                                                      474
5120     CONTINUE                                                         474
C    *                                                                    474
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  475
C    RAY_STRIPT)/2 <===                                                   475
         CALL AGSRESET                                                    475
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2       475
         AG_TITLE = 'BTRA'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'X'                                                 475
         CALL AXPOSITION                                                  475
C    *                                                                    475
C    Check TOF==2                                                         475
         IF (TOF.EQ.2) THEN                                               476
C    Create BRFE :                                                        477
C    Call To Local Procedure BRFE                                         477
         ASSIGN 5140 TO BLOCK_BRFE                                        477
         GO TO  5130                                                      477
5140     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  479
C    AY_HEIGHT+MODR_FEEH/2 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===                 479
         CALL AGSRESET                                                    479
         AG_X =                                                           479
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/2      479
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   479
         AG_TITLE = 'BRFE'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Z'                                               479
         CALL AXPOSITION                                                  479
C    *                                                                    479
         ENDIF                                                            479
5091  CALL AGSPOPB                                                        481
      GO TO BLOCK_BSEC, (5090 ,5100)                                      481
C End of Local Procedure                                                  481
C ----------------------------------------------------------------------  482
C ------------ BTRA                                                       482
C Begin Local Procedure BTRA                                              483
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             483
5110  CONTINUE                                                            483
         AG_TITLE='BTRA'                                                  483
         AG_MODULE=MODULE                                                 483
         AG_MARK='V00'                                                    483
         CALL AGSPUSH                                                     483
         AG_MODULE=MODULE                                                 483
C    *                                                                    483
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       484
         CALL AGSRESET                                                    484
         AG_SEEN = 1                                                      484
         AG_COLO = 2                                                      484
         AG_TITLE = 'BTRA'                                                484
         AG_EXNAME = 'ATTRIBUTE'                                          484
         AG_PARLIST = 'SEEN_COLO'                                         484
         CALL AXATTRIBUTE                                                 484
C    *                                                                    484
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  484
C    , DY=TRAY_WIDTH/2 <===                                               484
         CALL AGSRESET                                                    486
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                486
         AG_DY = TRAY_WIDTH/2                                             486
         AG_TITLE = 'BOX'                                                 486
         AG_EXNAME = 'SHAPE'                                              486
         AG_PARLIST = 'DX_DY'                                             486
         CALL AXSHAPE                                                     486
C    *                                                                    486
C    Create BXTR :                                                        487
C    Call To Local Procedure BXTR                                         487
         ASSIGN 5160 TO BLOCK_BXTR                                        487
         GO TO  5150                                                      487
5160     CONTINUE                                                         487
C    *                                                                    487
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  488
C    DZ-TRAY_LENGTH)/2 <===                                               488
         CALL AGSRESET                                                    488
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             488
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   488
         AG_TITLE = 'BXTR'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Z'                                               488
         CALL AXPOSITION                                                  488
C    *                                                                    488
C    Create BUND :                                                        489
C    Call To Local Procedure BUND                                         489
         ASSIGN 5180 TO BLOCK_BUND                                        489
         GO TO  5170                                                      489
5180     CONTINUE                                                         489
C    *                                                                    489
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  490
C    Z-TRAY_SUPLEN)/2 <===                                                490
         CALL AGSRESET                                                    490
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              490
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   490
         AG_TITLE = 'BUND'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Z'                                               490
         CALL AXPOSITION                                                  490
C    *                                                                    490
5111  CALL AGSPOPB                                                        491
      GO TO BLOCK_BTRA, (5110 ,5120)                                      491
C End of Local Procedure                                                  491
C ----------------------------------------------------------------------  492
C ------------ BRFE                                                       492
C Begin Local Procedure BRFE                                              493
C BRFE IS THE FEE OF TOFR (RUN-3)                                         493
5130  CONTINUE                                                            493
         AG_TITLE='BRFE'                                                  493
         AG_MODULE=MODULE                                                 493
         AG_MARK='V00'                                                    493
         CALL AGSPUSH                                                     493
         AG_MODULE=MODULE                                                 493
C    *                                                                    493
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       494
         CALL AGSRESET                                                    494
         AG_SEEN = 0                                                      494
         AG_COLO = 3                                                      494
         AG_TITLE = 'BRFE'                                                494
         AG_EXNAME = 'ATTRIBUTE'                                          494
         AG_PARLIST = 'SEEN_COLO'                                         494
         CALL AXATTRIBUTE                                                 494
C    *                                                                    494
C    Geant MATERIAL : G10 ===> inherited arguments used <===              494
         CALL AGSRESET                                                    495
         AG_TITLE = 'G10'                                                 495
         AG_EXNAME = 'MATERIAL'                                           495
         AG_PARLIST = 'NONE'                                              495
         CALL AXMATERIAL                                                  495
C    *                                                                    495
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  495
C    TH/2 <===                                                            495
         CALL AGSRESET                                                    497
         AG_DX = MODR_FEEH/2                                              497
         AG_DY = TRAY_WIDTH/2                                             497
         AG_DZ = TRAY_LENGTH/2                                            497
         AG_TITLE = 'BOX'                                                 497
         AG_EXNAME = 'SHAPE'                                              497
         AG_PARLIST = 'DX_DY_DZ'                                          497
         CALL AXSHAPE                                                     497
C    *                                                                    497
5131  CALL AGSPOPB                                                        498
      GO TO BLOCK_BRFE, (5130 ,5140)                                      498
C End of Local Procedure                                                  498
C ----------------------------------------------------------------------  499
C ------------ BXTR                                                       499
C Begin Local Procedure BXTR                                              500
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         500
5150  CONTINUE                                                            500
         AG_TITLE='BXTR'                                                  500
         AG_MODULE=MODULE                                                 500
         AG_MARK='V00'                                                    500
         CALL AGSPUSH                                                     500
         AG_MODULE=MODULE                                                 500
C    *                                                                    500
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       501
         CALL AGSRESET                                                    501
         AG_SEEN = 0                                                      501
         AG_COLO = 2                                                      501
         AG_TITLE = 'BXTR'                                                501
         AG_EXNAME = 'ATTRIBUTE'                                          501
         AG_PARLIST = 'SEEN_COLO'                                         501
         CALL AXATTRIBUTE                                                 501
C    *                                                                    501
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        501
         CALL AGSRESET                                                    502
         AG_TITLE = 'ALUMINIUM'                                           502
         AG_EXNAME = 'MATERIAL'                                           502
         AG_PARLIST = 'NONE'                                              502
         CALL AXMATERIAL                                                  502
C    *                                                                    502
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        502
         CALL AGSRESET                                                    503
         AG_DX = TRAY_HEIGHT/2                                            503
         AG_DZ = TRAY_LENGTH/2                                            503
         AG_TITLE = 'BOX'                                                 503
         AG_EXNAME = 'SHAPE'                                              503
         AG_PARLIST = 'DX_DZ'                                             503
         CALL AXSHAPE                                                     503
C    *                                                                    503
C    Check TOF==1                                                         503
         IF (TOF.EQ.1) THEN                                               504
C    Create BTTC :                                                        505
C    Call To Local Procedure BTTC                                         505
         ASSIGN 5200 TO BLOCK_BTTC                                        505
         GO TO  5190                                                      505
5200     CONTINUE                                                         505
C    *                                                                    505
C    Geant POSITION : BTTC ===> inherited arguments used <===             506
         CALL AGSRESET                                                    506
         AG_TITLE = 'BTTC'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'NONE'                                              506
         CALL AXPOSITION                                                  506
C    *                                                                    506
         ELSEIF (TOF.GE.2) THEN                                           506
C    Create BRTC :                                                        508
C    Call To Local Procedure BRTC                                         508
         ASSIGN 5220 TO BLOCK_BRTC                                        508
         GO TO  5210                                                      508
5220     CONTINUE                                                         508
C    *                                                                    508
C    Geant POSITION : BRTC ===> inherited arguments used <===             509
         CALL AGSRESET                                                    509
         AG_TITLE = 'BRTC'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'NONE'                                              509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    Create BUPC :                                                        510
C    Call To Local Procedure BUPC                                         510
         ASSIGN 5240 TO BLOCK_BUPC                                        510
         GO TO  5230                                                      510
5240     CONTINUE                                                         510
C    *                                                                    510
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_WALLTHK)/2 <===       511
         CALL AGSRESET                                                    511
         AG_X = (TRAY_HEIGHT-TRAY_WALLTHK)/2                              511
         AG_TITLE = 'BUPC'                                                511
         AG_EXNAME = 'POSITION'                                           511
         AG_PARLIST = 'X'                                                 511
         CALL AXPOSITION                                                  511
C    *                                                                    511
         ELSE                                                             511
C    Create BMTC :                                                        513
C    Call To Local Procedure BMTC                                         513
         ASSIGN 5260 TO BLOCK_BMTC                                        513
         GO TO  5250                                                      513
5260     CONTINUE                                                         513
C    *                                                                    513
C    Geant POSITION : BMTC ===> inherited arguments used <===             514
         CALL AGSRESET                                                    514
         AG_TITLE = 'BMTC'                                                514
         AG_EXNAME = 'POSITION'                                           514
         AG_PARLIST = 'NONE'                                              514
         CALL AXPOSITION                                                  514
C    *                                                                    514
         ENDIF                                                            514
5151  CALL AGSPOPB                                                        516
      GO TO BLOCK_BXTR, (5150 ,5160)                                      516
C End of Local Procedure                                                  516
C ----------------------------------------------------------------------  517
C ------------ BMTC                                                       517
C Begin Local Procedure BMTC                                              518
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            518
5250  CONTINUE                                                            518
         AG_TITLE='BMTC'                                                  518
         AG_MODULE=MODULE                                                 518
         AG_MARK='V00'                                                    518
         CALL AGSPUSH                                                     518
         AG_MODULE=MODULE                                                 518
C    *                                                                    518
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      519
         CALL AGSRESET                                                    519
         AG_SEEN = -1                                                     519
         AG_COLO = 5                                                      519
         AG_TITLE = 'BMTC'                                                519
         AG_EXNAME = 'ATTRIBUTE'                                          519
         AG_PARLIST = 'SEEN_COLO'                                         519
         CALL AXATTRIBUTE                                                 519
C    *                                                                    519
C    Geant MATERIAL : AIR ===> inherited arguments used <===              519
         CALL AGSRESET                                                    520
         AG_TITLE = 'AIR'                                                 520
         AG_EXNAME = 'MATERIAL'                                           520
         AG_PARLIST = 'NONE'                                              520
         CALL AXMATERIAL                                                  520
C    *                                                                    520
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  520
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   520
         CALL AGSRESET                                                    522
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               522
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                522
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               522
         AG_TITLE = 'BOX'                                                 522
         AG_EXNAME = 'SHAPE'                                              522
         AG_PARLIST = 'DX_DY_DZ'                                          522
         CALL AXSHAPE                                                     522
C    *                                                                    522
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           522
     *   CTBB_WRAP                                                        522
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             524
C    Create BXSA :                                                        525
C    Call To Local Procedure BXSA                                         525
         ASSIGN 5280 TO BLOCK_BXSA                                        525
         GO TO  5270                                                      525
5280     CONTINUE                                                         525
C    *                                                                    525
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  527
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   527
         CALL AGSRESET                                                    527
         AG_X = XPOS                                                      527
         AG_Z = ZPOS                                                      527
         AG_DZ = CTBB_SLAB1LEN/2                                          527
         AG_DX = CTBB_SLABTHCK/2                                          527
         AG_DY = CTBB_SLABWID/2                                           527
         AG_TITLE = 'BXSA'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      527
         CALL AXPOSITION                                                  527
C    *                                                                    527
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   527
C    Create BCCV :                                                        529
C    Call To Local Procedure BCCV                                         529
         ASSIGN 5300 TO BLOCK_BCCV                                        529
         GO TO  5290                                                      529
5300     CONTINUE                                                         529
C    *                                                                    529
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  531
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  531
C    K/2 <===                                                             531
         CALL AGSRESET                                                    531
         AG_X = XPOS                                                      531
         AG_Z = ZPOS                                                      531
         AG_DX2 = CTBB_SLABTHCK/2                                         531
         AG_DY2 = CTBB_SLABWID/2                                          531
         AG_DY1 = CTBB_CONVWIDM/2                                         531
         AG_DZ = CTBB_CONVLEN/2                                           531
         AG_DX1 = CTBB_SLABTHCK/2                                         531
         AG_TITLE = 'BCCV'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            531
         CALL AXPOSITION                                                  531
C    *                                                                    531
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     531
C    Create BCPM :                                                        533
C    Call To Local Procedure BCPM                                         533
         ASSIGN 5320 TO BLOCK_BCPM                                        533
         GO TO  5310                                                      533
5320     CONTINUE                                                         533
C    *                                                                    533
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  535
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      535
         CALL AGSRESET                                                    535
         AG_X = XPOS                                                      535
         AG_Z = ZPOS                                                      535
         AG_RMIN = CTBB_PMTMINR                                           535
         AG_RMAX = CTBB_PMTMAXR                                           535
         AG_DZ = CTBB_PMTLEN/2                                            535
         AG_TITLE = 'BCPM'                                                535
         AG_EXNAME = 'POSITION'                                           535
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  535
         CALL AXPOSITION                                                  535
C    *                                                                    535
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     535
C    Create BCSK :                                                        537
C    Call To Local Procedure BCSK                                         537
         ASSIGN 5340 TO BLOCK_BCSK                                        537
         GO TO  5330                                                      537
5340     CONTINUE                                                         537
C    *                                                                    537
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  539
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   539
         CALL AGSRESET                                                    539
         AG_X = XPOS                                                      539
         AG_Z = ZPOS                                                      539
         AG_RMIN = CTBB_BASEMINR                                          539
         AG_RMAX = CTBB_BASEMAXR                                          539
         AG_DZ = CTBB_BASELEN/2                                           539
         AG_TITLE = 'BCSK'                                                539
         AG_EXNAME = 'POSITION'                                           539
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  539
         CALL AXPOSITION                                                  539
C    *                                                                    539
C    Create BZEL :                                                        540
C    Call To Local Procedure BZEL                                         540
         ASSIGN 5360 TO BLOCK_BZEL                                        540
         GO TO  5350                                                      540
5360     CONTINUE                                                         540
C    *                                                                    540
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  542
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 542
         CALL AGSRESET                                                    542
         AG_X = XPOS                                                      542
         AG_Z = ZPOS                                                      542
         AG_DZ = CTBB_BASELEN/2                                           542
         AG_DX = CTBB_ELECTHCK/2                                          542
         AG_DY = CTBB_BASEMINR-0.1                                        542
         AG_TITLE = 'BZEL'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      542
         CALL AXPOSITION                                                  542
C    *                                                                    542
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           542
     *   CTBB_WRAP - CTBB_SHIM                                            542
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             544
C    Create BXSA :                                                        545
C    Call To Local Procedure BXSA                                         545
         ASSIGN 5370 TO BLOCK_BXSA                                        545
         GO TO  5270                                                      545
5370     CONTINUE                                                         545
C    *                                                                    545
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  547
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  547
         CALL AGSRESET                                                    547
         AG_X = XPOS                                                      547
         AG_Z = -ZPOS                                                     547
         AG_DZ = CTBB_SLAB2LEN/2                                          547
         AG_DX = CTBB_SLABTHCK/2                                          547
         AG_DY = CTBB_SLABWID/2                                           547
         AG_TITLE = 'BXSA'                                                547
         AG_EXNAME = 'POSITION'                                           547
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      547
         CALL AXPOSITION                                                  547
C    *                                                                    547
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   547
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  550
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  550
C    CK/2 ALPHAX=180 <===                                                 550
         CALL AGSRESET                                                    550
         AG_X = XPOS                                                      550
         AG_Z = -ZPOS                                                     550
         AG_DX2 = CTBB_SLABTHCK/2                                         550
         AG_DY2 = CTBB_SLABWID/2                                          550
         AG_DY1 = CTBB_CONVWIDM/2                                         550
         AG_DZ = CTBB_CONVLEN/2                                           550
         AG_DX1 = CTBB_SLABTHCK/2                                         550
         AG_ALPHAN=AG_ALPHAN+1                                            550
         AG_ALPHAK(AG_ALPHAN)=1                                           550
         AG_ALPHAL(AG_ALPHAN)=180                                         550
         AG_TITLE = 'BCCV'                                                550
         AG_EXNAME = 'POSITION'                                           550
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     550
         CALL AXPOSITION                                                  550
C    *                                                                    550
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     550
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  553
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     553
         CALL AGSRESET                                                    553
         AG_X = XPOS                                                      553
         AG_Z = -ZPOS                                                     553
         AG_RMIN = CTBB_PMTMINR                                           553
         AG_RMAX = CTBB_PMTMAXR                                           553
         AG_DZ = CTBB_PMTLEN/2                                            553
         AG_TITLE = 'BCPM'                                                553
         AG_EXNAME = 'POSITION'                                           553
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  553
         CALL AXPOSITION                                                  553
C    *                                                                    553
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     553
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  556
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  556
         CALL AGSRESET                                                    556
         AG_X = XPOS                                                      556
         AG_Z = -ZPOS                                                     556
         AG_RMIN = CTBB_BASEMINR                                          556
         AG_RMAX = CTBB_BASEMAXR                                          556
         AG_DZ = CTBB_BASELEN/2                                           556
         AG_TITLE = 'BCSK'                                                556
         AG_EXNAME = 'POSITION'                                           556
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  556
         CALL AXPOSITION                                                  556
C    *                                                                    556
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  556
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                556
         CALL AGSRESET                                                    558
         AG_X = XPOS                                                      558
         AG_Z = -ZPOS                                                     558
         AG_DZ = CTBB_BASELEN/2                                           558
         AG_DX = CTBB_ELECTHCK/2                                          558
         AG_DY = CTBB_BASEMINR-0.1                                        558
         AG_TITLE = 'BZEL'                                                558
         AG_EXNAME = 'POSITION'                                           558
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      558
         CALL AXPOSITION                                                  558
C    *                                                                    558
5251  CALL AGSPOPB                                                        559
      GO TO BLOCK_BMTC, (5250 ,5260)                                      559
C End of Local Procedure                                                  559
C ----------------------------------------------------------------------  560
C ------------ BTTC                                                       560
C Begin Local Procedure BTTC                                              561
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           561
5190  CONTINUE                                                            561
         AG_TITLE='BTTC'                                                  561
         AG_MODULE=MODULE                                                 561
         AG_MARK='V00'                                                    561
         CALL AGSPUSH                                                     561
         AG_MODULE=MODULE                                                 561
C    *                                                                    561
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       562
         CALL AGSRESET                                                    562
         AG_SEEN = 0                                                      562
         AG_COLO = 3                                                      562
         AG_TITLE = 'BTTC'                                                562
         AG_EXNAME = 'ATTRIBUTE'                                          562
         AG_PARLIST = 'SEEN_COLO'                                         562
         CALL AXATTRIBUTE                                                 562
C    *                                                                    562
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           562
         CALL AGSRESET                                                    563
         AG_A = 12                                                        563
         AG_Z = 6                                                         563
         AG_W = 1                                                         563
         AG_TITLE = 'C'                                                   563
         AG_EXNAME = 'COMPONENT'                                          563
         AG_PARLIST = 'A_Z_W'                                             563
         CALL AXCOMPONENT                                                 563
C    *                                                                    563
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           563
         CALL AGSRESET                                                    564
         AG_A = 1                                                         564
         AG_Z = 1                                                         564
         AG_W = 2                                                         564
         AG_TITLE = 'H2'                                                  564
         AG_EXNAME = 'COMPONENT'                                          564
         AG_PARLIST = 'A_Z_W'                                             564
         CALL AXCOMPONENT                                                 564
C    *                                                                    564
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       564
         CALL AGSRESET                                                    565
         AG_DENS = 0.048                                                  565
         AG_TITLE = 'LASTAFOAM'                                           565
         AG_EXNAME = 'MIXTURE'                                            565
         AG_PARLIST = 'DENS'                                              565
         CALL AXMIXTURE                                                   565
C    *                                                                    565
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  565
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   565
         CALL AGSRESET                                                    567
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               567
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                567
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               567
         AG_TITLE = 'BOX'                                                 567
         AG_EXNAME = 'SHAPE'                                              567
         AG_PARLIST = 'DX_DY_DZ'                                          567
         CALL AXSHAPE                                                     567
C    *                                                                    567
         SUBLEN =((TOFF_SLATZ(2) + TOFF_SLAT5Z) - (TOFF_SLATZ(10) -       567
     *   TOFF_SLAT5Z))                                                    567
         SUBCEN =(TOFF_SLATZ(2) + TOFF_SLAT5Z) - SUBLEN/2.                569
         IWID=4                                                           570
C    Create BMAA :                                                        571
C    Call To Local Procedure BMAA                                         571
         ASSIGN 5390 TO BLOCK_BMAA                                        571
         GO TO  5380                                                      571
5390     CONTINUE                                                         571
C    *                                                                    571
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            572
         CALL AGSRESET                                                    572
         AG_X = 0                                                         572
         AG_Z = SUBCEN                                                    572
         AG_KONLY = 'MANY'                                                572
         AG_TITLE = 'BMAA'                                                572
         AG_EXNAME = 'POSITION'                                           572
         AG_PARLIST = 'X_Z_KONLY'                                         572
         CALL AXPOSITION                                                  572
C    *                                                                    572
         IWID=5                                                           572
C    Create BMAA :                                                        574
C    Call To Local Procedure BMAA                                         574
         ASSIGN 5400 TO BLOCK_BMAA                                        574
         GO TO  5380                                                      574
5400     CONTINUE                                                         574
C    *                                                                    574
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLATZ(1) KONLY='MANY' <===   575
         CALL AGSRESET                                                    575
         AG_X = 0.0                                                       575
         AG_Z = TOFF_SLATZ(1)                                             575
         AG_KONLY = 'MANY'                                                575
         AG_TITLE = 'BMAA'                                                575
         AG_EXNAME = 'POSITION'                                           575
         AG_PARLIST = 'X_Z_KONLY'                                         575
         CALL AXPOSITION                                                  575
C    *                                                                    575
C    Create BCOO :                                                        576
C    Call To Local Procedure BCOO                                         576
         ASSIGN 5420 TO BLOCK_BCOO                                        576
         GO TO  5410                                                      576
5420     CONTINUE                                                         576
C    *                                                                    576
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               577
         CALL AGSRESET                                                    577
         AG_X = 0                                                         577
         AG_Y = 0                                                         577
         AG_DZ = 0                                                        577
         AG_DX = 0                                                        577
         AG_DY = 0                                                        577
         AG_TITLE = 'BCOO'                                                577
         AG_EXNAME = 'POSITION'                                           577
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      577
         CALL AXPOSITION                                                  577
C    *                                                                    577
         AG_DZ = TOFF_ELECLEN/2                                           577
         AG_DX = TOFF_ELECTHCK/2                                          578
         AG_DY = TOFF_ELECWID/2                                           578
C    Create BFEE :                                                        578
C    Call To Local Procedure BFEE                                         578
         ASSIGN 5440 TO BLOCK_BFEE                                        578
         GO TO  5430                                                      578
5440     CONTINUE                                                         578
C    *                                                                    578
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(1)-TOFF_ELEC  579
C    LEN/2 <===                                                           579
         CALL AGSRESET                                                    579
         AG_X = TOFF_ELECHGT                                              579
         AG_Z = TOFF_ELECZ(1)-TOFF_ELECLEN/2                              579
         AG_TITLE = 'BFEE'                                                579
         AG_EXNAME = 'POSITION'                                           579
         AG_PARLIST = 'X_Z'                                               579
         CALL AXPOSITION                                                  579
C    *                                                                    579
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(2)-TOFF_ELEC  579
C    LEN/2 <===                                                           579
         CALL AGSRESET                                                    580
         AG_X = TOFF_ELECHGT                                              580
         AG_Z = TOFF_ELECZ(2)-TOFF_ELECLEN/2                              580
         AG_TITLE = 'BFEE'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'X_Z'                                               580
         CALL AXPOSITION                                                  580
C    *                                                                    580
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(3)-TOFF_ELEC  580
C    LEN/2 <===                                                           580
         CALL AGSRESET                                                    581
         AG_X = TOFF_ELECHGT                                              581
         AG_Z = TOFF_ELECZ(3)-TOFF_ELECLEN/2                              581
         AG_TITLE = 'BFEE'                                                581
         AG_EXNAME = 'POSITION'                                           581
         AG_PARLIST = 'X_Z'                                               581
         CALL AXPOSITION                                                  581
C    *                                                                    581
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(4)-TOFF_ELEC  581
C    LEN/2 <===                                                           581
         CALL AGSRESET                                                    582
         AG_X = TOFF_ELECHGT                                              582
         AG_Z = TOFF_ELECZ(4)-TOFF_ELECLEN/2                              582
         AG_TITLE = 'BFEE'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'X_Z'                                               582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(5)-TOFF_ELEC  582
C    LEN/2 <===                                                           582
         CALL AGSRESET                                                    583
         AG_X = TOFF_ELECHGT                                              583
         AG_Z = TOFF_ELECZ(5)-TOFF_ELECLEN/2                              583
         AG_TITLE = 'BFEE'                                                583
         AG_EXNAME = 'POSITION'                                           583
         AG_PARLIST = 'X_Z'                                               583
         CALL AXPOSITION                                                  583
C    *                                                                    583
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(6)-TOFF_ELEC  583
C    LEN/2 <===                                                           583
         CALL AGSRESET                                                    584
         AG_X = TOFF_ELECHGT                                              584
         AG_Z = TOFF_ELECZ(6)-TOFF_ELECLEN/2                              584
         AG_TITLE = 'BFEE'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'X_Z'                                               584
         CALL AXPOSITION                                                  584
C    *                                                                    584
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(7)-TOFF_ELEC  584
C    LEN/2 <===                                                           584
         CALL AGSRESET                                                    585
         AG_X = TOFF_ELECHGT                                              585
         AG_Z = TOFF_ELECZ(7)-TOFF_ELECLEN/2                              585
         AG_TITLE = 'BFEE'                                                585
         AG_EXNAME = 'POSITION'                                           585
         AG_PARLIST = 'X_Z'                                               585
         CALL AXPOSITION                                                  585
C    *                                                                    585
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(8)-TOFF_ELEC  585
C    LEN/2 <===                                                           585
         CALL AGSRESET                                                    586
         AG_X = TOFF_ELECHGT                                              586
         AG_Z = TOFF_ELECZ(8)-TOFF_ELECLEN/2                              586
         AG_TITLE = 'BFEE'                                                586
         AG_EXNAME = 'POSITION'                                           586
         AG_PARLIST = 'X_Z'                                               586
         CALL AXPOSITION                                                  586
C    *                                                                    586
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(9)-TOFF_ELEC  586
C    LEN/2 <===                                                           586
         CALL AGSRESET                                                    587
         AG_X = TOFF_ELECHGT                                              587
         AG_Z = TOFF_ELECZ(9)-TOFF_ELECLEN/2                              587
         AG_TITLE = 'BFEE'                                                587
         AG_EXNAME = 'POSITION'                                           587
         AG_PARLIST = 'X_Z'                                               587
         CALL AXPOSITION                                                  587
C    *                                                                    587
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(10)-TOFF_ELE  587
C    CLEN/2 <===                                                          587
         CALL AGSRESET                                                    588
         AG_X = TOFF_ELECHGT                                              588
         AG_Z = TOFF_ELECZ(10)-TOFF_ELECLEN/2                             588
         AG_TITLE = 'BFEE'                                                588
         AG_EXNAME = 'POSITION'                                           588
         AG_PARLIST = 'X_Z'                                               588
         CALL AXPOSITION                                                  588
C    *                                                                    588
C    Create BPLA :                                                        589
C    Call To Local Procedure BPLA                                         589
         ASSIGN 5460 TO BLOCK_BPLA                                        589
         GO TO  5450                                                      589
5460     CONTINUE                                                         589
C    *                                                                    589
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(1)+TOFF_  590
C    PLASPOS <===                                                         590
         CALL AGSRESET                                                    590
         AG_X = TOFF_ELECHGT                                              590
         AG_Y = 0                                                         590
         AG_Z = TOFF_ELECZ(1)+TOFF_PLASPOS                                590
         AG_TITLE = 'BPLA'                                                590
         AG_EXNAME = 'POSITION'                                           590
         AG_PARLIST = 'X_Y_Z'                                             590
         CALL AXPOSITION                                                  590
C    *                                                                    590
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(2)+TOFF_  590
C    PLASPOS <===                                                         590
         CALL AGSRESET                                                    591
         AG_X = TOFF_ELECHGT                                              591
         AG_Y = 0                                                         591
         AG_Z = TOFF_ELECZ(2)+TOFF_PLASPOS                                591
         AG_TITLE = 'BPLA'                                                591
         AG_EXNAME = 'POSITION'                                           591
         AG_PARLIST = 'X_Y_Z'                                             591
         CALL AXPOSITION                                                  591
C    *                                                                    591
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(3)+TOFF_  591
C    PLASPOS <===                                                         591
         CALL AGSRESET                                                    592
         AG_X = TOFF_ELECHGT                                              592
         AG_Y = 0                                                         592
         AG_Z = TOFF_ELECZ(3)+TOFF_PLASPOS                                592
         AG_TITLE = 'BPLA'                                                592
         AG_EXNAME = 'POSITION'                                           592
         AG_PARLIST = 'X_Y_Z'                                             592
         CALL AXPOSITION                                                  592
C    *                                                                    592
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(4)+TOFF_  592
C    PLASPOS <===                                                         592
         CALL AGSRESET                                                    593
         AG_X = TOFF_ELECHGT                                              593
         AG_Y = 0                                                         593
         AG_Z = TOFF_ELECZ(4)+TOFF_PLASPOS                                593
         AG_TITLE = 'BPLA'                                                593
         AG_EXNAME = 'POSITION'                                           593
         AG_PARLIST = 'X_Y_Z'                                             593
         CALL AXPOSITION                                                  593
C    *                                                                    593
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(5)+TOFF_  593
C    PLASPOS <===                                                         593
         CALL AGSRESET                                                    594
         AG_X = TOFF_ELECHGT                                              594
         AG_Y = 0                                                         594
         AG_Z = TOFF_ELECZ(5)+TOFF_PLASPOS                                594
         AG_TITLE = 'BPLA'                                                594
         AG_EXNAME = 'POSITION'                                           594
         AG_PARLIST = 'X_Y_Z'                                             594
         CALL AXPOSITION                                                  594
C    *                                                                    594
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(6)+TOFF_  594
C    PLASPOS <===                                                         594
         CALL AGSRESET                                                    595
         AG_X = TOFF_ELECHGT                                              595
         AG_Y = 0                                                         595
         AG_Z = TOFF_ELECZ(6)+TOFF_PLASPOS                                595
         AG_TITLE = 'BPLA'                                                595
         AG_EXNAME = 'POSITION'                                           595
         AG_PARLIST = 'X_Y_Z'                                             595
         CALL AXPOSITION                                                  595
C    *                                                                    595
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(7)+TOFF_  595
C    PLASPOS <===                                                         595
         CALL AGSRESET                                                    596
         AG_X = TOFF_ELECHGT                                              596
         AG_Y = 0                                                         596
         AG_Z = TOFF_ELECZ(7)+TOFF_PLASPOS                                596
         AG_TITLE = 'BPLA'                                                596
         AG_EXNAME = 'POSITION'                                           596
         AG_PARLIST = 'X_Y_Z'                                             596
         CALL AXPOSITION                                                  596
C    *                                                                    596
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(8)+TOFF_  596
C    PLASPOS <===                                                         596
         CALL AGSRESET                                                    597
         AG_X = TOFF_ELECHGT                                              597
         AG_Y = 0                                                         597
         AG_Z = TOFF_ELECZ(8)+TOFF_PLASPOS                                597
         AG_TITLE = 'BPLA'                                                597
         AG_EXNAME = 'POSITION'                                           597
         AG_PARLIST = 'X_Y_Z'                                             597
         CALL AXPOSITION                                                  597
C    *                                                                    597
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(9)+TOFF_  597
C    PLASPOS <===                                                         597
         CALL AGSRESET                                                    598
         AG_X = TOFF_ELECHGT                                              598
         AG_Y = 0                                                         598
         AG_Z = TOFF_ELECZ(9)+TOFF_PLASPOS                                598
         AG_TITLE = 'BPLA'                                                598
         AG_EXNAME = 'POSITION'                                           598
         AG_PARLIST = 'X_Y_Z'                                             598
         CALL AXPOSITION                                                  598
C    *                                                                    598
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(10)+TOFF  598
C    _PLASPOS <===                                                        598
         CALL AGSRESET                                                    599
         AG_X = TOFF_ELECHGT                                              599
         AG_Y = 0                                                         599
         AG_Z = TOFF_ELECZ(10)+TOFF_PLASPOS                               599
         AG_TITLE = 'BPLA'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Y_Z'                                             599
         CALL AXPOSITION                                                  599
C    *                                                                    599
5191  CALL AGSPOPB                                                        600
      GO TO BLOCK_BTTC, (5190 ,5200)                                      600
C End of Local Procedure                                                  600
C ----------------------------------------------------------------------  601
C ------------ BUPC                                                       601
C Begin Local Procedure BUPC                                              602
C BUPC IS THE UP PCB COVER OF TOFR                                        602
5230  CONTINUE                                                            602
         AG_TITLE='BUPC'                                                  602
         AG_MODULE=MODULE                                                 602
         AG_MARK='V00'                                                    602
         CALL AGSPUSH                                                     602
         AG_MODULE=MODULE                                                 602
C    *                                                                    602
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       603
         CALL AGSRESET                                                    603
         AG_SEEN = 0                                                      603
         AG_COLO = 3                                                      603
         AG_TITLE = 'BUPC'                                                603
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
C    Geant SHAPE : BOX ===> DX=TRAY_WALLTHK/2 DY=TRAY_WIDTH/2-TRAY_WALLT  604
C    HK, DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                               604
         CALL AGSRESET                                                    606
         AG_DX = TRAY_WALLTHK/2                                           606
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                606
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               606
         AG_TITLE = 'BOX'                                                 606
         AG_EXNAME = 'SHAPE'                                              606
         AG_PARLIST = 'DX_DY_DZ'                                          606
         CALL AXSHAPE                                                     606
C    *                                                                    606
5231  CALL AGSPOPB                                                        607
      GO TO BLOCK_BUPC, (5230 ,5240)                                      607
C End of Local Procedure                                                  607
C ----------------------------------------------------------------------  608
C ------------ BRTC                                                       608
C Begin Local Procedure BRTC                                              610
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  610
C R RUN4)                                                                 610
5210  CONTINUE                                                            610
         AG_TITLE='BRTC'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       611
         CALL AGSRESET                                                    611
         AG_SEEN = 0                                                      611
         AG_COLO = 5                                                      611
         AG_TITLE = 'BRTC'                                                611
         AG_EXNAME = 'ATTRIBUTE'                                          611
         AG_PARLIST = 'SEEN_COLO'                                         611
         CALL AXATTRIBUTE                                                 611
C    *                                                                    611
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        611
         CALL AGSRESET                                                    612
         AG_TITLE = 'HONEYCOMB'                                           612
         AG_EXNAME = 'MATERIAL'                                           612
         AG_PARLIST = 'NONE'                                              612
         CALL AXMATERIAL                                                  612
C    *                                                                    612
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  612
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   612
         CALL AGSRESET                                                    614
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               614
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                614
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               614
         AG_TITLE = 'BOX'                                                 614
         AG_EXNAME = 'SHAPE'                                              614
         AG_PARLIST = 'DX_DY_DZ'                                          614
         CALL AXSHAPE                                                     614
C    *                                                                    614
C    Create BGMT :                                                        615
C    Call To Local Procedure BGMT                                         615
         ASSIGN 5480 TO BLOCK_BGMT                                        615
         GO TO  5470                                                      615
5480     CONTINUE                                                         615
C    *                                                                    615
C    Geant POSITION : BGMT ===> KONLY='MANY' <===                         616
         CALL AGSRESET                                                    616
         AG_KONLY = 'MANY'                                                616
         AG_TITLE = 'BGMT'                                                616
         AG_EXNAME = 'POSITION'                                           616
         AG_PARLIST = 'KONLY'                                             616
         CALL AXPOSITION                                                  616
C    *                                                                    616
C    Create BRMD :                                                        617
C    Call To Local Procedure BRMD                                         617
         ASSIGN 5500 TO BLOCK_BRMD                                        617
         GO TO  5490                                                      617
5500     CONTINUE                                                         617
C    *                                                                    617
C    Check TOF==2                                                         618
         IF (TOF.EQ.2) THEN                                               618
         Z0 = TRAY_LENGTH/2 - 0.05                                        619
         X0 = MODR_X0OFFSET                                               620
C    *                                                                    621
         I=1                                                              621
            GO TO 5513                                                    621
5511        I=I+1                                                         621
5513        IF(I-(33).GT.0) GO TO 5512                                    621
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  622
C        ALPHAY=MODR_MRPCA(I) <===                                        622
            CALL AGSRESET                                                 622
            AG_X = X0+MODR_MRPCX(I)                                       622
            AG_Z = Z0-MODR_MRPCZ(I)                                       622
            AG_ALPHAN=AG_ALPHAN+1                                         622
            AG_ALPHAK(AG_ALPHAN)=2                                        622
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            622
            AG_TITLE = 'BRMD'                                             622
            AG_EXNAME = 'POSITION'                                        622
            AG_PARLIST = 'X_Z_ALPHAY'                                     622
            CALL AXPOSITION                                               622
C       *                                                                 622
         GO TO 5511                                                       623
5512     CONTINUE                                                         623
         ELSEIF (TOF.EQ.3) THEN                                           624
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       625
         X0 = MOD4_X0OFFSET                                               626
C    *                                                                    627
         I=1                                                              627
            GO TO 5523                                                    627
5521        I=I+1                                                         627
5523        IF(I-(32).GT.0) GO TO 5522                                    627
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            628
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    628
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  629
C        ALPHAY=MOD4_MRPCA(I) <===                                        629
            CALL AGSRESET                                                 629
            AG_X = X0+MOD4_MRPCX(I)                                       629
            AG_Z = Z0-MOD4_MRPCZ(I)                                       629
            AG_ALPHAN=AG_ALPHAN+1                                         629
            AG_ALPHAK(AG_ALPHAN)=2                                        629
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            629
            AG_TITLE = 'BRMD'                                             629
            AG_EXNAME = 'POSITION'                                        629
            AG_PARLIST = 'X_Z_ALPHAY'                                     629
            CALL AXPOSITION                                               629
C       *                                                                 629
            ENDIF                                                         629
         GO TO 5521                                                       631
5522     CONTINUE                                                         631
         ELSEIF (TOF.EQ.4) THEN                                           632
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD5_TRAYEDGEZ                       633
         X0 = MOD5_X0OFFSET                                               634
C    *                                                                    635
         I=1                                                              635
            GO TO 5533                                                    635
5531        I=I+1                                                         635
5533        IF(I-(32).GT.0) GO TO 5532                                    635
C       Geant POSITION : BRMD ===> X=X0+MOD5_MRPCX(I) Z=Z0-MOD5_MRPCZ(I)  636
C        ALPHAY=MOD5_MRPCA(I) <===                                        636
            CALL AGSRESET                                                 636
            AG_X = X0+MOD5_MRPCX(I)                                       636
            AG_Z = Z0-MOD5_MRPCZ(I)                                       636
            AG_ALPHAN=AG_ALPHAN+1                                         636
            AG_ALPHAK(AG_ALPHAN)=2                                        636
            AG_ALPHAL(AG_ALPHAN)=MOD5_MRPCA(I)                            636
            AG_TITLE = 'BRMD'                                             636
            AG_EXNAME = 'POSITION'                                        636
            AG_PARLIST = 'X_Z_ALPHAY'                                     636
            CALL AXPOSITION                                               636
C       *                                                                 636
         GO TO 5531                                                       637
5532     CONTINUE                                                         637
         ELSEIF (TOF.EQ.5) THEN                                           638
         ELSEIF (TOF.EQ.6) THEN                                           639
         Z0 = TRAY_LENGTH/2 - MOD7_TRAYEDGEZ                              640
         X0 = -TRAY_HEIGHT/2 + TRAY_WALLTHK                               641
C    *                                                                    642
         I=1                                                              642
            GO TO 5543                                                    642
5541        I=I+1                                                         642
5543        IF(I-(32).GT.0) GO TO 5542                                    642
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  643
C        ALPHAY=MOD7_MRPCA(I) <===                                        643
            CALL AGSRESET                                                 643
            AG_X = X0+MOD7_MRPCX(I)                                       643
            AG_Z = Z0-MOD7_MRPCZ(I)                                       643
            AG_ALPHAN=AG_ALPHAN+1                                         643
            AG_ALPHAK(AG_ALPHAN)=2                                        643
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            643
            AG_TITLE = 'BRMD'                                             643
            AG_EXNAME = 'POSITION'                                        643
            AG_PARLIST = 'X_Z_ALPHAY'                                     643
            CALL AXPOSITION                                               643
C       *                                                                 643
         GO TO 5541                                                       644
5542     CONTINUE                                                         644
         ELSEIF (TOF.EQ.7) THEN                                           645
         Z0 = TRAY_LENGTH/2 - MOD7_TRAYEDGEZ                              646
         X0 = -TRAY_HEIGHT/2 + TRAY_WALLTHK                               647
C    *                                                                    648
         I=1                                                              648
            GO TO 5553                                                    648
5551        I=I+1                                                         648
5553        IF(I-(32).GT.0) GO TO 5552                                    648
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  649
C        ALPHAY=MOD7_MRPCA(I) <===                                        649
            CALL AGSRESET                                                 649
            AG_X = X0+MOD7_MRPCX(I)                                       649
            AG_Z = Z0-MOD7_MRPCZ(I)                                       649
            AG_ALPHAN=AG_ALPHAN+1                                         649
            AG_ALPHAK(AG_ALPHAN)=2                                        649
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            649
            AG_TITLE = 'BRMD'                                             649
            AG_EXNAME = 'POSITION'                                        649
            AG_PARLIST = 'X_Z_ALPHAY'                                     649
            CALL AXPOSITION                                               649
C       *                                                                 649
         GO TO 5551                                                       650
5552     CONTINUE                                                         650
         ENDIF                                                            651
5211  CALL AGSPOPB                                                        652
      GO TO BLOCK_BRTC, (5210 ,5220)                                      652
C End of Local Procedure                                                  652
C ----------------------------------------------------------------------  653
C ------------ BGMT                                                       653
C Begin Local Procedure BGMT                                              654
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    654
5470  CONTINUE                                                            654
         AG_TITLE='BGMT'                                                  654
         AG_MODULE=MODULE                                                 654
         AG_MARK='V00'                                                    654
         CALL AGSPUSH                                                     654
         AG_MODULE=MODULE                                                 654
C    *                                                                    654
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       655
         CALL AGSRESET                                                    655
         AG_SEEN = 0                                                      655
         AG_COLO = 2                                                      655
         AG_TITLE = 'BGMT'                                                655
         AG_EXNAME = 'ATTRIBUTE'                                          655
         AG_PARLIST = 'SEEN_COLO'                                         655
         CALL AXATTRIBUTE                                                 655
C    *                                                                    655
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           655
         CALL AGSRESET                                                    656
         AG_TITLE = 'RPCGAS'                                              656
         AG_EXNAME = 'MATERIAL'                                           656
         AG_PARLIST = 'NONE'                                              656
         CALL AXMATERIAL                                                  656
C    *                                                                    656
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  656
C    /2-TRAY_WALLTHK-MODR_HBWID DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===        656
         CALL AGSRESET                                                    658
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               658
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-MODR_HBWID                     658
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               658
         AG_TITLE = 'BOX'                                                 658
         AG_EXNAME = 'SHAPE'                                              658
         AG_PARLIST = 'DX_DY_DZ'                                          658
         CALL AXSHAPE                                                     658
C    *                                                                    658
5471  CALL AGSPOPB                                                        659
      GO TO BLOCK_BGMT, (5470 ,5480)                                      659
C End of Local Procedure                                                  659
C ----------------------------------------------------------------------  660
C ------------ BMAA                                                       660
C Begin Local Procedure BMAA                                              661
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    661
5380  CONTINUE                                                            661
         AG_TITLE='BMAA'                                                  661
         AG_MODULE=MODULE                                                 661
         AG_MARK='V00'                                                    661
         CALL AGSPUSH                                                     661
         AG_MODULE=MODULE                                                 661
C    *                                                                    661
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       662
         CALL AGSRESET                                                    662
         AG_SEEN = 0                                                      662
         AG_COLO = 2                                                      662
         AG_TITLE = 'BMAA'                                                662
         AG_EXNAME = 'ATTRIBUTE'                                          662
         AG_PARLIST = 'SEEN_COLO'                                         662
         CALL AXATTRIBUTE                                                 662
C    *                                                                    662
C    Check IWID==4                                                        662
         IF (IWID.EQ.4) THEN                                              663
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  665
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          665
         CALL AGSRESET                                                    665
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               665
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      665
         AG_DZ = SUBLEN/2.                                                665
         AG_TITLE = 'BOX'                                                 665
         AG_EXNAME = 'SHAPE'                                              665
         AG_PARLIST = 'DX_DY_DZ'                                          665
         CALL AXSHAPE                                                     665
C    *                                                                    665
         ELSE                                                             665
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  668
C    DTH/2 DZ=TOFF_SLAT5Z <===                                            668
         CALL AGSRESET                                                    668
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               668
         AG_DY = TOFF_BOXWIDTH/2                                          668
         AG_DZ = TOFF_SLAT5Z                                              668
         AG_TITLE = 'BOX'                                                 668
         AG_EXNAME = 'SHAPE'                                              668
         AG_PARLIST = 'DX_DY_DZ'                                          668
         CALL AXSHAPE                                                     668
C    *                                                                    668
         ENDIF                                                            668
C    Create BMTD :                                                        670
C    Call To Local Procedure BMTD                                         670
         ASSIGN 5570 TO BLOCK_BMTD                                        670
         GO TO  5560                                                      670
5570     CONTINUE                                                         670
C    *                                                                    670
5381  CALL AGSPOPB                                                        671
      GO TO BLOCK_BMAA, (5380 ,5390,5400)                                 671
C End of Local Procedure                                                  671
C ----------------------------------------------------------------------  672
C ------------ BMTD                                                       672
C Begin Local Procedure BMTD                                              673
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         673
5560  CONTINUE                                                            673
         AG_TITLE='BMTD'                                                  673
         AG_MODULE=MODULE                                                 673
         AG_MARK='V00'                                                    673
         CALL AGSPUSH                                                     673
         AG_MODULE=MODULE                                                 673
C    *                                                                    673
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       674
         CALL AGSRESET                                                    674
         AG_SEEN = 1                                                      674
         AG_COLO = 1                                                      674
         AG_TITLE = 'BMTD'                                                674
         AG_EXNAME = 'ATTRIBUTE'                                          674
         AG_PARLIST = 'SEEN_COLO'                                         674
         CALL AXATTRIBUTE                                                 674
C    *                                                                    674
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   674
         CALL AGSRESET                                                    675
         AG_NDIV = IWID                                                   675
         AG_IAXIS = 2                                                     675
         AG_TITLE = 'DIVISION'                                            675
         AG_EXNAME = 'SHAPE'                                              675
         AG_PARLIST = 'NDIV_IAXIS'                                        675
         CALL AXSHAPE                                                     675
C    *                                                                    675
C    Create BASS :                                                        676
C    Call To Local Procedure BASS                                         676
         ASSIGN 5590 TO BLOCK_BASS                                        676
         GO TO  5580                                                      676
5590     CONTINUE                                                         676
C    *                                                                    676
C    Check IWID==5                                                        677
         IF (IWID.EQ.5) THEN                                              677
C    Geant POSITION : BASS ===> X=TOFF_SLATX(1) ALPHAY=TOFF_SLATAY(1) KO  678
C    NLY='MANY' <===                                                      678
         CALL AGSRESET                                                    678
         AG_X = TOFF_SLATX(1)                                             678
         AG_ALPHAN=AG_ALPHAN+1                                            678
         AG_ALPHAK(AG_ALPHAN)=2                                           678
         AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(1)                              678
         AG_KONLY = 'MANY'                                                678
         AG_TITLE = 'BASS'                                                678
         AG_EXNAME = 'POSITION'                                           678
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    678
         CALL AXPOSITION                                                  678
C    *                                                                    678
         ELSE                                                             678
C    *                                                                    680
         ISLAT=2                                                          680
            GO TO 5603                                                    680
5601        ISLAT=ISLAT+1                                                 680
5603        IF(ISLAT-(10).GT.0) GO TO 5602                                680
C       Geant POSITION : BASS ===> X=TOFF_SLATX(ISLAT) Z=TOFF_SLATZ(ISLA  682
C       T)-SUBCEN ALPHAY=TOFF_SLATAY(ISLAT) KONLY='MANY' <===             682
            CALL AGSRESET                                                 682
            AG_X = TOFF_SLATX(ISLAT)                                      682
            AG_Z = TOFF_SLATZ(ISLAT)-SUBCEN                               682
            AG_ALPHAN=AG_ALPHAN+1                                         682
            AG_ALPHAK(AG_ALPHAN)=2                                        682
            AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(ISLAT)                       682
            AG_KONLY = 'MANY'                                             682
            AG_TITLE = 'BASS'                                             682
            AG_EXNAME = 'POSITION'                                        682
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               682
            CALL AXPOSITION                                               682
C       *                                                                 682
         GO TO 5601                                                       683
5602     CONTINUE                                                         683
         ENDIF                                                            684
5561  CALL AGSPOPB                                                        685
      GO TO BLOCK_BMTD, (5560 ,5570)                                      685
C End of Local Procedure                                                  685
C ----------------------------------------------------------------------  686
C ------------ BASS                                                       686
C Begin Local Procedure BASS                                              687
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      687
5580  CONTINUE                                                            687
         AG_TITLE='BASS'                                                  687
         AG_MODULE=MODULE                                                 687
         AG_MARK='V00'                                                    687
         CALL AGSPUSH                                                     687
         AG_MODULE=MODULE                                                 687
C    *                                                                    687
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       688
         CALL AGSRESET                                                    688
         AG_SEEN = 0                                                      688
         AG_COLO = 6                                                      688
         AG_TITLE = 'BASS'                                                688
         AG_EXNAME = 'ATTRIBUTE'                                          688
         AG_PARLIST = 'SEEN_COLO'                                         688
         CALL AXATTRIBUTE                                                 688
C    *                                                                    688
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               688
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  691
C    K)/5., DZ=TOTLEN/2. <===                                             691
         CALL AGSRESET                                                    691
         AG_DX = TOFF_PMTMAXR                                             691
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           691
         AG_DZ = TOTLEN/2.                                                691
         AG_TITLE = 'BOX'                                                 691
         AG_EXNAME = 'SHAPE'                                              691
         AG_PARLIST = 'DX_DY_DZ'                                          691
         CALL AXSHAPE                                                     691
C    *                                                                    691
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               691
C    Create BCSB :                                                        693
C    Call To Local Procedure BCSB                                         693
         ASSIGN 5620 TO BLOCK_BCSB                                        693
         GO TO  5610                                                      693
5620     CONTINUE                                                         693
C    *                                                                    693
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               694
         CALL AGSRESET                                                    694
         AG_Z = ZPOS                                                      694
         AG_TITLE = 'BCSB'                                                694
         AG_EXNAME = 'POSITION'                                           694
         AG_PARLIST = 'Z'                                                 694
         CALL AXPOSITION                                                  694
C    *                                                                    694
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     694
C    Create BCPM :                                                        696
C    Call To Local Procedure BCPM                                         696
         ASSIGN 5630 TO BLOCK_BCPM                                        696
         GO TO  5310                                                      696
5630     CONTINUE                                                         696
C    *                                                                    696
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  697
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         697
         CALL AGSRESET                                                    697
         AG_X = 0                                                         697
         AG_Z = ZPOS                                                      697
         AG_RMIN = TOFF_PMTMINR                                           697
         AG_RMAX = TOFF_PMTMAXR                                           697
         AG_DZ = TOFF_PMTLEN/2                                            697
         AG_TITLE = 'BCPM'                                                697
         AG_EXNAME = 'POSITION'                                           697
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  697
         CALL AXPOSITION                                                  697
C    *                                                                    697
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     697
C    Create BTSK :                                                        699
C    Call To Local Procedure BTSK                                         699
         ASSIGN 5650 TO BLOCK_BTSK                                        699
         GO TO  5640                                                      699
5650     CONTINUE                                                         699
C    *                                                                    699
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  701
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       701
         CALL AGSRESET                                                    701
         AG_X = 0                                                         701
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     701
         AG_RMIN = TOFF_PMTMINR                                           701
         AG_RMAX = TOFF_PMTMAXR                                           701
         AG_DZ = TOFF_SOCKLEN/2                                           701
         AG_TITLE = 'BTSK'                                                701
         AG_EXNAME = 'POSITION'                                           701
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  701
         CALL AXPOSITION                                                  701
C    *                                                                    701
C    Create BCEL :                                                        702
C    Call To Local Procedure BCEL                                         702
         ASSIGN 5670 TO BLOCK_BCEL                                        702
         GO TO  5660                                                      702
5670     CONTINUE                                                         702
C    *                                                                    702
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  704
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                704
         CALL AGSRESET                                                    704
         AG_X = 0                                                         704
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    704
         AG_RMIN = 0                                                      704
         AG_RMAX = TOFF_PMTMINR                                           704
         AG_DZ = TOFF_ELECTHCK/2                                          704
         AG_TITLE = 'BCEL'                                                704
         AG_EXNAME = 'POSITION'                                           704
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  704
         CALL AXPOSITION                                                  704
C    *                                                                    704
         ZPOS = ZPOS + TOFF_BASELEN/2                                     704
         AG_DZ = TOFF_BASELEN/2                                           706
         AG_DX = TOFF_ELECTHCK/2                                          706
         AG_DY = TOFF_CELLWID/2                                           706
C    Create BCEB :                                                        706
C    Call To Local Procedure BCEB                                         706
         ASSIGN 5690 TO BLOCK_BCEB                                        706
         GO TO  5680                                                      706
5690     CONTINUE                                                         706
C    *                                                                    706
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              707
         CALL AGSRESET                                                    707
         AG_X = TOFF_CELLHGT/2                                            707
         AG_Z = ZPOS                                                      707
         AG_TITLE = 'BCEB'                                                707
         AG_EXNAME = 'POSITION'                                           707
         AG_PARLIST = 'X_Z'                                               707
         CALL AXPOSITION                                                  707
C    *                                                                    707
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             707
         CALL AGSRESET                                                    708
         AG_X = -TOFF_CELLHGT/2                                           708
         AG_Z = ZPOS                                                      708
         AG_TITLE = 'BCEB'                                                708
         AG_EXNAME = 'POSITION'                                           708
         AG_PARLIST = 'X_Z'                                               708
         CALL AXPOSITION                                                  708
C    *                                                                    708
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               708
C    Create BCON :                                                        710
C    Call To Local Procedure BCON                                         710
         ASSIGN 5710 TO BLOCK_BCON                                        710
         GO TO  5700                                                      710
5710     CONTINUE                                                         710
C    *                                                                    710
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=TOFF_BCONZLEN/2 DX=(TO  712
C    FF_CELLHGT-TOFF_ELECTHCK)/2 DY=TOFF_BCONYLEN/2 <===                  712
         CALL AGSRESET                                                    712
         AG_X = 0                                                         712
         AG_Y = 0                                                         712
         AG_Z = ZPOS                                                      712
         AG_DZ = TOFF_BCONZLEN/2                                          712
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           712
         AG_DY = TOFF_BCONYLEN/2                                          712
         AG_TITLE = 'BCON'                                                712
         AG_EXNAME = 'POSITION'                                           712
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    712
         CALL AXPOSITION                                                  712
C    *                                                                    712
5581  CALL AGSPOPB                                                        713
      GO TO BLOCK_BASS, (5580 ,5590)                                      713
C End of Local Procedure                                                  713
C ----------------------------------------------------------------------  714
C ------------ BXSA                                                       714
C Begin Local Procedure BXSA                                              715
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    715
5270  CONTINUE                                                            715
         AG_TITLE='BXSA'                                                  715
         AG_MODULE=MODULE                                                 715
         AG_MARK='V00'                                                    715
         CALL AGSPUSH                                                     715
         AG_MODULE=MODULE                                                 715
C    *                                                                    715
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       716
         CALL AGSRESET                                                    716
         AG_SEEN = 0                                                      716
         AG_COLO = 3                                                      716
         AG_TITLE = 'BXSA'                                                716
         AG_EXNAME = 'ATTRIBUTE'                                          716
         AG_PARLIST = 'SEEN_COLO'                                         716
         CALL AXATTRIBUTE                                                 716
C    *                                                                    716
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       716
         CALL AGSRESET                                                    717
         AG_TITLE = 'POLYSTYREN'                                          717
         AG_EXNAME = 'MATERIAL'                                           717
         AG_PARLIST = 'NONE'                                              717
         CALL AXMATERIAL                                                  717
C    *                                                                    717
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           717
         CALL AGSRESET                                                    718
         AG_ISVOL = 1                                                     718
         AG_TITLE = 'SENSITIVE'                                           718
         AG_EXNAME = 'MEDIUM'                                             718
         AG_PARLIST = 'ISVOL'                                             718
         CALL AXMEDIUM                                                    718
C    *                                                                    718
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           718
         CALL AGSRESET                                                    719
         AG_DX = 0                                                        719
         AG_DY = 0                                                        719
         AG_DZ = 0                                                        719
         AG_TITLE = 'BOX'                                                 719
         AG_EXNAME = 'SHAPE'                                              719
         AG_PARLIST = 'DX_DY_DZ'                                          719
         CALL AXSHAPE                                                     719
C    *                                                                    719
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   719
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  719
C    SS:16:(0.0,0.01)                                                     719
         AG_TITLE = 'BXSA'                                                721
         AG_EXNAME = 'HITS'                                               721
         AG_TYPE=AG_MAGIC                                                 721
         CALL AGSDETV('BXSA')                                             721
C    *                                                                    721
         AG_PARLIST='X'                                                   721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = AG_MAGIC                                              721
         AG_BIN=0.01                                                      721
         AG_OPTION='S'                                                    721
         AG_FHMIN = AG_MAGIC                                              721
         AG_FHMAX = AG_MAGIC                                              721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='Y'                                                   721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = AG_MAGIC                                              721
         AG_BIN=0.01                                                      721
         AG_FHMIN=AG_MAGIC                                                721
         AG_FHMAX=AG_MAGIC                                                721
         AG_OPTION='*'                                                    721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='Z'                                                   721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = AG_MAGIC                                              721
         AG_BIN=0.01                                                      721
         AG_FHMIN=AG_MAGIC                                                721
         AG_FHMAX=AG_MAGIC                                                721
         AG_OPTION='*'                                                    721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='PTOT'                                                721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = 18                                                    721
         AG_BIN=AG_MAGIC                                                  721
         AG_OPTION='*'                                                    721
         AG_FHMIN = 0.0                                                   721
         AG_FHMAX = 100.0                                                 721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='CX'                                                  721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = 10                                                    721
         AG_BIN=AG_MAGIC                                                  721
         AG_FHMIN=AG_MAGIC                                                721
         AG_FHMAX=AG_MAGIC                                                721
         AG_OPTION='*'                                                    721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='CY'                                                  721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = 10                                                    721
         AG_BIN=AG_MAGIC                                                  721
         AG_FHMIN=AG_MAGIC                                                721
         AG_FHMAX=AG_MAGIC                                                721
         AG_OPTION='*'                                                    721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='CZ'                                                  721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = 10                                                    721
         AG_BIN=AG_MAGIC                                                  721
         AG_OPTION='*'                                                    721
         AG_FHMIN = AG_MAGIC                                              721
         AG_FHMAX = AG_MAGIC                                              721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='SLENG'                                               721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = AG_MAGIC                                              721
         AG_BIN=0.1                                                       721
         AG_OPTION='*'                                                    721
         AG_FHMIN = 0                                                     721
         AG_FHMAX = 500                                                   721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='TOF'                                                 721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = 16                                                    721
         AG_BIN=AG_MAGIC                                                  721
         AG_OPTION='*'                                                    721
         AG_FHMIN = 0.0                                                   721
         AG_FHMAX = 1.0E-6                                                721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='STEP'                                                721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = AG_MAGIC                                              721
         AG_BIN=0.01                                                      721
         AG_FHMIN=AG_MAGIC                                                721
         AG_FHMAX=AG_MAGIC                                                721
         AG_OPTION='*'                                                    721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         AG_PARLIST='ELOSS'                                               721
         AG_TITLE = 'DETH'                                                721
         AG_NBITS = 16                                                    721
         AG_BIN=AG_MAGIC                                                  721
         AG_OPTION='*'                                                    721
         AG_FHMIN = 0.0                                                   721
         AG_FHMAX = 0.01                                                  721
         CALL AGSDETH (0)                                                 721
C    *                                                                    721
         CALL AGSDETU('BXSA')                                             721
C    *                                                                    721
5271  CALL AGSPOPB                                                        722
      GO TO BLOCK_BXSA, (5270 ,5280,5370)                                 722
C End of Local Procedure                                                  722
C ----------------------------------------------------------------------  723
C ------------ BCSB                                                       723
C Begin Local Procedure BCSB                                              724
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    724
5610  CONTINUE                                                            724
         AG_TITLE='BCSB'                                                  724
         AG_MODULE=MODULE                                                 724
         AG_MARK='V00'                                                    724
         CALL AGSPUSH                                                     724
         AG_MODULE=MODULE                                                 724
C    *                                                                    724
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       725
         CALL AGSRESET                                                    725
         AG_SEEN = 1                                                      725
         AG_COLO = 7                                                      725
         AG_TITLE = 'BCSB'                                                725
         AG_EXNAME = 'ATTRIBUTE'                                          725
         AG_PARLIST = 'SEEN_COLO'                                         725
         CALL AXATTRIBUTE                                                 725
C    *                                                                    725
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       725
         CALL AGSRESET                                                    726
         AG_TITLE = 'POLYSTYREN'                                          726
         AG_EXNAME = 'MATERIAL'                                           726
         AG_PARLIST = 'NONE'                                              726
         CALL AXMATERIAL                                                  726
C    *                                                                    726
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           726
         CALL AGSRESET                                                    727
         AG_ISVOL = 1                                                     727
         AG_TITLE = 'SENSITIVE'                                           727
         AG_EXNAME = 'MEDIUM'                                             727
         AG_PARLIST = 'ISVOL'                                             727
         CALL AXMEDIUM                                                    727
C    *                                                                    727
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  727
C    F_SLATLEN/2 <===                                                     727
         CALL AGSRESET                                                    729
         AG_DX = TOFF_SLATTHCK/2                                          729
         AG_DY = TOFF_SLATWID/2                                           729
         AG_DZ = TOFF_SLATLEN/2                                           729
         AG_TITLE = 'BOX'                                                 729
         AG_EXNAME = 'SHAPE'                                              729
         AG_PARLIST = 'DX_DY_DZ'                                          729
         CALL AXSHAPE                                                     729
C    *                                                                    729
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   729
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  729
C    SS:16:(0.0,0.01)                                                     729
         AG_TITLE = 'BCSB'                                                731
         AG_EXNAME = 'HITS'                                               731
         AG_TYPE=AG_MAGIC                                                 731
         CALL AGSDETV('BCSB')                                             731
C    *                                                                    731
         AG_PARLIST='X'                                                   731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = AG_MAGIC                                              731
         AG_BIN=0.01                                                      731
         AG_OPTION='S'                                                    731
         AG_FHMIN = AG_MAGIC                                              731
         AG_FHMAX = AG_MAGIC                                              731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='Y'                                                   731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = AG_MAGIC                                              731
         AG_BIN=0.01                                                      731
         AG_FHMIN=AG_MAGIC                                                731
         AG_FHMAX=AG_MAGIC                                                731
         AG_OPTION='*'                                                    731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='Z'                                                   731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = AG_MAGIC                                              731
         AG_BIN=0.01                                                      731
         AG_FHMIN=AG_MAGIC                                                731
         AG_FHMAX=AG_MAGIC                                                731
         AG_OPTION='*'                                                    731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='PTOT'                                                731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = 18                                                    731
         AG_BIN=AG_MAGIC                                                  731
         AG_OPTION='*'                                                    731
         AG_FHMIN = 0.0                                                   731
         AG_FHMAX = 100.0                                                 731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='CX'                                                  731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = 10                                                    731
         AG_BIN=AG_MAGIC                                                  731
         AG_FHMIN=AG_MAGIC                                                731
         AG_FHMAX=AG_MAGIC                                                731
         AG_OPTION='*'                                                    731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='CY'                                                  731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = 10                                                    731
         AG_BIN=AG_MAGIC                                                  731
         AG_FHMIN=AG_MAGIC                                                731
         AG_FHMAX=AG_MAGIC                                                731
         AG_OPTION='*'                                                    731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='CZ'                                                  731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = 10                                                    731
         AG_BIN=AG_MAGIC                                                  731
         AG_OPTION='*'                                                    731
         AG_FHMIN = AG_MAGIC                                              731
         AG_FHMAX = AG_MAGIC                                              731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='SLENG'                                               731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = AG_MAGIC                                              731
         AG_BIN=0.1                                                       731
         AG_OPTION='*'                                                    731
         AG_FHMIN = 0                                                     731
         AG_FHMAX = 500                                                   731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='TOF'                                                 731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = 16                                                    731
         AG_BIN=AG_MAGIC                                                  731
         AG_OPTION='*'                                                    731
         AG_FHMIN = 0.0                                                   731
         AG_FHMAX = 1.0E-6                                                731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='STEP'                                                731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = AG_MAGIC                                              731
         AG_BIN=0.01                                                      731
         AG_FHMIN=AG_MAGIC                                                731
         AG_FHMAX=AG_MAGIC                                                731
         AG_OPTION='*'                                                    731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         AG_PARLIST='ELOSS'                                               731
         AG_TITLE = 'DETH'                                                731
         AG_NBITS = 16                                                    731
         AG_BIN=AG_MAGIC                                                  731
         AG_OPTION='*'                                                    731
         AG_FHMIN = 0.0                                                   731
         AG_FHMAX = 0.01                                                  731
         CALL AGSDETH (0)                                                 731
C    *                                                                    731
         CALL AGSDETU('BCSB')                                             731
C    *                                                                    731
5611  CALL AGSPOPB                                                        732
      GO TO BLOCK_BCSB, (5610 ,5620)                                      732
C End of Local Procedure                                                  732
C ----------------------------------------------------------------------  733
C ------------ BCCV                                                       733
C Begin Local Procedure BCCV                                              734
C BCCV IS A CTB OPTICAL CONVERTER                                         734
5290  CONTINUE                                                            734
         AG_TITLE='BCCV'                                                  734
         AG_MODULE=MODULE                                                 734
         AG_MARK='V00'                                                    734
         CALL AGSPUSH                                                     734
         AG_MODULE=MODULE                                                 734
C    *                                                                    734
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       735
         CALL AGSRESET                                                    735
         AG_SEEN = 0                                                      735
         AG_COLO = 3                                                      735
         AG_TITLE = 'BCCV'                                                735
         AG_EXNAME = 'ATTRIBUTE'                                          735
         AG_PARLIST = 'SEEN_COLO'                                         735
         CALL AXATTRIBUTE                                                 735
C    *                                                                    735
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       735
         CALL AGSRESET                                                    736
         AG_TITLE = 'POLYSTYREN'                                          736
         AG_EXNAME = 'MATERIAL'                                           736
         AG_PARLIST = 'NONE'                                              736
         CALL AXMATERIAL                                                  736
C    *                                                                    736
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            736
         CALL AGSRESET                                                    737
         AG_DX1 = 0                                                       737
         AG_DX2 = 0                                                       737
         AG_DY1 = 0                                                       737
         AG_DY2 = 0                                                       737
         AG_DZ = 0                                                        737
         AG_TITLE = 'TRD2'                                                737
         AG_EXNAME = 'SHAPE'                                              737
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                737
         CALL AXSHAPE                                                     737
C    *                                                                    737
5291  CALL AGSPOPB                                                        738
      GO TO BLOCK_BCCV, (5290 ,5300)                                      738
C End of Local Procedure                                                  738
C ----------------------------------------------------------------------  739
C ------------ BCSK                                                       739
C Begin Local Procedure BCSK                                              740
C BCSK IS A CTB LINEAR BASE TUBE                                          740
5330  CONTINUE                                                            740
         AG_TITLE='BCSK'                                                  740
         AG_MODULE=MODULE                                                 740
         AG_MARK='V00'                                                    740
         CALL AGSPUSH                                                     740
         AG_MODULE=MODULE                                                 740
C    *                                                                    740
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       741
         CALL AGSRESET                                                    741
         AG_SEEN = 0                                                      741
         AG_COLO = 2                                                      741
         AG_TITLE = 'BCSK'                                                741
         AG_EXNAME = 'ATTRIBUTE'                                          741
         AG_PARLIST = 'SEEN_COLO'                                         741
         CALL AXATTRIBUTE                                                 741
C    *                                                                    741
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       741
         CALL AGSRESET                                                    742
         AG_TITLE = 'POLYSTYREN'                                          742
         AG_EXNAME = 'MATERIAL'                                           742
         AG_PARLIST = 'NONE'                                              742
         CALL AXMATERIAL                                                  742
C    *                                                                    742
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      742
         CALL AGSRESET                                                    743
         AG_RMIN = 0                                                      743
         AG_RMAX = 0                                                      743
         AG_DZ = 0                                                        743
         AG_TITLE = 'TUBE'                                                743
         AG_EXNAME = 'SHAPE'                                              743
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      743
         CALL AXSHAPE                                                     743
C    *                                                                    743
5331  CALL AGSPOPB                                                        744
      GO TO BLOCK_BCSK, (5330 ,5340)                                      744
C End of Local Procedure                                                  744
C ----------------------------------------------------------------------  745
C ------------ BZEL                                                       745
C Begin Local Procedure BZEL                                              746
C BZEL IS A CTB PM ELECTRONICS                                            746
5350  CONTINUE                                                            746
         AG_TITLE='BZEL'                                                  746
         AG_MODULE=MODULE                                                 746
         AG_MARK='V00'                                                    746
         CALL AGSPUSH                                                     746
         AG_MODULE=MODULE                                                 746
C    *                                                                    746
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       747
         CALL AGSRESET                                                    747
         AG_SEEN = 0                                                      747
         AG_COLO = 6                                                      747
         AG_TITLE = 'BZEL'                                                747
         AG_EXNAME = 'ATTRIBUTE'                                          747
         AG_PARLIST = 'SEEN_COLO'                                         747
         CALL AXATTRIBUTE                                                 747
C    *                                                                    747
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          747
         CALL AGSRESET                                                    748
         AG_TITLE = 'SILICON'                                             748
         AG_EXNAME = 'MATERIAL'                                           748
         AG_PARLIST = 'NONE'                                              748
         CALL AXMATERIAL                                                  748
C    *                                                                    748
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           748
         CALL AGSRESET                                                    749
         AG_DX = 0                                                        749
         AG_DY = 0                                                        749
         AG_DZ = 0                                                        749
         AG_TITLE = 'BOX'                                                 749
         AG_EXNAME = 'SHAPE'                                              749
         AG_PARLIST = 'DX_DY_DZ'                                          749
         CALL AXSHAPE                                                     749
C    *                                                                    749
5351  CALL AGSPOPB                                                        750
      GO TO BLOCK_BZEL, (5350 ,5360)                                      750
C End of Local Procedure                                                  750
C ----------------------------------------------------------------------  751
C ------------ BCPM                                                       751
C Begin Local Procedure BCPM                                              752
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   752
5310  CONTINUE                                                            752
         AG_TITLE='BCPM'                                                  752
         AG_MODULE=MODULE                                                 752
         AG_MARK='V00'                                                    752
         CALL AGSPUSH                                                     752
         AG_MODULE=MODULE                                                 752
C    *                                                                    752
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       753
         CALL AGSRESET                                                    753
         AG_SEEN = 0                                                      753
         AG_COLO = 1                                                      753
         AG_TITLE = 'BCPM'                                                753
         AG_EXNAME = 'ATTRIBUTE'                                          753
         AG_PARLIST = 'SEEN_COLO'                                         753
         CALL AXATTRIBUTE                                                 753
C    *                                                                    753
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       753
         CALL AGSRESET                                                    754
         AG_TITLE = 'POLYSTYREN'                                          754
         AG_EXNAME = 'MATERIAL'                                           754
         AG_PARLIST = 'NONE'                                              754
         CALL AXMATERIAL                                                  754
C    *                                                                    754
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      754
         CALL AGSRESET                                                    755
         AG_RMIN = 0                                                      755
         AG_RMAX = 0                                                      755
         AG_DZ = 0                                                        755
         AG_TITLE = 'TUBE'                                                755
         AG_EXNAME = 'SHAPE'                                              755
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      755
         CALL AXSHAPE                                                     755
C    *                                                                    755
5311  CALL AGSPOPB                                                        756
      GO TO BLOCK_BCPM, (5310 ,5320,5630)                                 756
C End of Local Procedure                                                  756
C ----------------------------------------------------------------------  757
C ------------ BTSK                                                       757
C Begin Local Procedure BTSK                                              758
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                758
5640  CONTINUE                                                            758
         AG_TITLE='BTSK'                                                  758
         AG_MODULE=MODULE                                                 758
         AG_MARK='V00'                                                    758
         CALL AGSPUSH                                                     758
         AG_MODULE=MODULE                                                 758
C    *                                                                    758
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       759
         CALL AGSRESET                                                    759
         AG_SEEN = 0                                                      759
         AG_COLO = 7                                                      759
         AG_TITLE = 'BTSK'                                                759
         AG_EXNAME = 'ATTRIBUTE'                                          759
         AG_PARLIST = 'SEEN_COLO'                                         759
         CALL AXATTRIBUTE                                                 759
C    *                                                                    759
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       759
         CALL AGSRESET                                                    760
         AG_TITLE = 'POLYSTYREN'                                          760
         AG_EXNAME = 'MATERIAL'                                           760
         AG_PARLIST = 'NONE'                                              760
         CALL AXMATERIAL                                                  760
C    *                                                                    760
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      760
         CALL AGSRESET                                                    761
         AG_RMIN = 0                                                      761
         AG_RMAX = 0                                                      761
         AG_DZ = 0                                                        761
         AG_TITLE = 'TUBE'                                                761
         AG_EXNAME = 'SHAPE'                                              761
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      761
         CALL AXSHAPE                                                     761
C    *                                                                    761
5641  CALL AGSPOPB                                                        762
      GO TO BLOCK_BTSK, (5640 ,5650)                                      762
C End of Local Procedure                                                  762
C ----------------------------------------------------------------------  763
C ------------ BCEL                                                       763
C Begin Local Procedure BCEL                                              764
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     764
5660  CONTINUE                                                            764
         AG_TITLE='BCEL'                                                  764
         AG_MODULE=MODULE                                                 764
         AG_MARK='V00'                                                    764
         CALL AGSPUSH                                                     764
         AG_MODULE=MODULE                                                 764
C    *                                                                    764
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       765
         CALL AGSRESET                                                    765
         AG_SEEN = 0                                                      765
         AG_COLO = 3                                                      765
         AG_TITLE = 'BCEL'                                                765
         AG_EXNAME = 'ATTRIBUTE'                                          765
         AG_PARLIST = 'SEEN_COLO'                                         765
         CALL AXATTRIBUTE                                                 765
C    *                                                                    765
C    Geant MATERIAL : G10 ===> inherited arguments used <===              765
         CALL AGSRESET                                                    766
         AG_TITLE = 'G10'                                                 766
         AG_EXNAME = 'MATERIAL'                                           766
         AG_PARLIST = 'NONE'                                              766
         CALL AXMATERIAL                                                  766
C    *                                                                    766
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      766
         CALL AGSRESET                                                    767
         AG_RMIN = 0                                                      767
         AG_RMAX = 0                                                      767
         AG_DZ = 0                                                        767
         AG_TITLE = 'TUBE'                                                767
         AG_EXNAME = 'SHAPE'                                              767
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      767
         CALL AXSHAPE                                                     767
C    *                                                                    767
5661  CALL AGSPOPB                                                        768
      GO TO BLOCK_BCEL, (5660 ,5670)                                      768
C End of Local Procedure                                                  768
C ----------------------------------------------------------------------  769
C ------------ BCEB                                                       769
C Begin Local Procedure BCEB                                              770
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       770
5680  CONTINUE                                                            770
         AG_TITLE='BCEB'                                                  770
         AG_MODULE=MODULE                                                 770
         AG_MARK='V00'                                                    770
         CALL AGSPUSH                                                     770
         AG_MODULE=MODULE                                                 770
C    *                                                                    770
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       771
         CALL AGSRESET                                                    771
         AG_SEEN = 0                                                      771
         AG_COLO = 3                                                      771
         AG_TITLE = 'BCEL'                                                771
         AG_EXNAME = 'ATTRIBUTE'                                          771
         AG_PARLIST = 'SEEN_COLO'                                         771
         CALL AXATTRIBUTE                                                 771
C    *                                                                    771
C    Geant MATERIAL : G10 ===> inherited arguments used <===              771
         CALL AGSRESET                                                    772
         AG_TITLE = 'G10'                                                 772
         AG_EXNAME = 'MATERIAL'                                           772
         AG_PARLIST = 'NONE'                                              772
         CALL AXMATERIAL                                                  772
C    *                                                                    772
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           772
         CALL AGSRESET                                                    773
         AG_DX = 0                                                        773
         AG_DY = 0                                                        773
         AG_DZ = 0                                                        773
         AG_TITLE = 'BOX'                                                 773
         AG_EXNAME = 'SHAPE'                                              773
         AG_PARLIST = 'DX_DY_DZ'                                          773
         CALL AXSHAPE                                                     773
C    *                                                                    773
5681  CALL AGSPOPB                                                        774
      GO TO BLOCK_BCEB, (5680 ,5690)                                      774
C End of Local Procedure                                                  774
C ----------------------------------------------------------------------  775
C ------------ BPLA                                                       775
C Begin Local Procedure BPLA                                              776
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   776
5450  CONTINUE                                                            776
         AG_TITLE='BPLA'                                                  776
         AG_MODULE=MODULE                                                 776
         AG_MARK='V00'                                                    776
         CALL AGSPUSH                                                     776
         AG_MODULE=MODULE                                                 776
C    *                                                                    776
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       777
         CALL AGSRESET                                                    777
         AG_SEEN = 0                                                      777
         AG_COLO = 4                                                      777
         AG_TITLE = 'BPLA'                                                777
         AG_EXNAME = 'ATTRIBUTE'                                          777
         AG_PARLIST = 'SEEN_COLO'                                         777
         CALL AXATTRIBUTE                                                 777
C    *                                                                    777
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       777
         CALL AGSRESET                                                    778
         AG_TITLE = 'POLYSTYREN'                                          778
         AG_EXNAME = 'MATERIAL'                                           778
         AG_PARLIST = 'NONE'                                              778
         CALL AXMATERIAL                                                  778
C    *                                                                    778
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           778
         CALL AGSRESET                                                    779
         AG_DX = 0                                                        779
         AG_DY = 0                                                        779
         AG_DZ = 0                                                        779
         AG_TITLE = 'BOX'                                                 779
         AG_EXNAME = 'SHAPE'                                              779
         AG_PARLIST = 'DX_DY_DZ'                                          779
         CALL AXSHAPE                                                     779
C    *                                                                    779
C    Create BCON :                                                        780
C    Call To Local Procedure BCON                                         780
         ASSIGN 5720 TO BLOCK_BCON                                        780
         GO TO  5700                                                      780
5720     CONTINUE                                                         780
C    *                                                                    780
C    Geant POSITION : BCON ===> X=0 Y=0 Z=-TOFF_BCONPLDZ DZ=TOFF_BCONPLD  782
C    Z DX=TOFF_BCONPLDX/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===           782
         CALL AGSRESET                                                    782
         AG_X = 0                                                         782
         AG_Y = 0                                                         782
         AG_Z = -TOFF_BCONPLDZ                                            782
         AG_DZ = TOFF_BCONPLDZ                                            782
         AG_DX = TOFF_BCONPLDX/2                                          782
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            782
         AG_TITLE = 'BCON'                                                782
         AG_EXNAME = 'POSITION'                                           782
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    782
         CALL AXPOSITION                                                  782
C    *                                                                    782
C    Geant POSITION : BCON ===> X=(-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2 Y=0 Z  782
C    =(-TOFF_BCONPLDX)/2 DZ=TOFF_BCONPLDX/2 DX=TOFF_BCONPLDZ/2 DY=TRAY_W  782
C    IDTH/2-TRAY_WALLTHK-2.0 <===                                         782
         CALL AGSRESET                                                    784
         AG_X = (-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2                          784
         AG_Y = 0                                                         784
         AG_Z = (-TOFF_BCONPLDX)/2                                        784
         AG_DZ = TOFF_BCONPLDX/2                                          784
         AG_DX = TOFF_BCONPLDZ/2                                          784
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            784
         AG_TITLE = 'BCON'                                                784
         AG_EXNAME = 'POSITION'                                           784
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    784
         CALL AXPOSITION                                                  784
C    *                                                                    784
5451  CALL AGSPOPB                                                        785
      GO TO BLOCK_BPLA, (5450 ,5460)                                      785
C End of Local Procedure                                                  785
C ----------------------------------------------------------------------  786
C ------------ BCON                                                       786
C Begin Local Procedure BCON                                              788
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  788
C NGLES, ETC......                                                        788
5700  CONTINUE                                                            788
         AG_TITLE='BCON'                                                  788
         AG_MODULE=MODULE                                                 788
         AG_MARK='V00'                                                    788
         CALL AGSPUSH                                                     788
         AG_MODULE=MODULE                                                 788
C    *                                                                    788
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       789
         CALL AGSRESET                                                    789
         AG_SEEN = 0                                                      789
         AG_COLO = 6                                                      789
         AG_TITLE = 'BCON'                                                789
         AG_EXNAME = 'ATTRIBUTE'                                          789
         AG_PARLIST = 'SEEN_COLO'                                         789
         CALL AXATTRIBUTE                                                 789
C    *                                                                    789
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       789
         CALL AGSRESET                                                    790
         AG_TITLE = 'POLYSTYREN'                                          790
         AG_EXNAME = 'MATERIAL'                                           790
         AG_PARLIST = 'NONE'                                              790
         CALL AXMATERIAL                                                  790
C    *                                                                    790
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           790
         CALL AGSRESET                                                    791
         AG_DX = 0                                                        791
         AG_DY = 0                                                        791
         AG_DZ = 0                                                        791
         AG_TITLE = 'BOX'                                                 791
         AG_EXNAME = 'SHAPE'                                              791
         AG_PARLIST = 'DX_DY_DZ'                                          791
         CALL AXSHAPE                                                     791
C    *                                                                    791
5701  CALL AGSPOPB                                                        792
      GO TO BLOCK_BCON, (5700 ,5710,5720)                                 792
C End of Local Procedure                                                  792
C ----------------------------------------------------------------------  793
C ------------ BFEE                                                       793
C Begin Local Procedure BFEE                                              794
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         794
5430  CONTINUE                                                            794
         AG_TITLE='BFEE'                                                  794
         AG_MODULE=MODULE                                                 794
         AG_MARK='V00'                                                    794
         CALL AGSPUSH                                                     794
         AG_MODULE=MODULE                                                 794
C    *                                                                    794
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       795
         CALL AGSRESET                                                    795
         AG_SEEN = 0                                                      795
         AG_COLO = 3                                                      795
         AG_TITLE = 'BFEE'                                                795
         AG_EXNAME = 'ATTRIBUTE'                                          795
         AG_PARLIST = 'SEEN_COLO'                                         795
         CALL AXATTRIBUTE                                                 795
C    *                                                                    795
C    Geant MATERIAL : G10 ===> inherited arguments used <===              795
         CALL AGSRESET                                                    796
         AG_TITLE = 'G10'                                                 796
         AG_EXNAME = 'MATERIAL'                                           796
         AG_PARLIST = 'NONE'                                              796
         CALL AXMATERIAL                                                  796
C    *                                                                    796
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           796
         CALL AGSRESET                                                    797
         AG_DX = 0                                                        797
         AG_DY = 0                                                        797
         AG_DZ = 0                                                        797
         AG_TITLE = 'BOX'                                                 797
         AG_EXNAME = 'SHAPE'                                              797
         AG_PARLIST = 'DX_DY_DZ'                                          797
         CALL AXSHAPE                                                     797
C    *                                                                    797
C    Create BLEM :                                                        798
C    Call To Local Procedure BLEM                                         798
         ASSIGN 5740 TO BLOCK_BLEM                                        798
         GO TO  5730                                                      798
5740     CONTINUE                                                         798
C    *                                                                    798
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(1) Z=TOF  799
C    F_BLEMPOSZ1 <===                                                     799
         CALL AGSRESET                                                    799
         AG_X = TOFF_BLEMPOSX                                             799
         AG_Y = TOFF_BLEMPOSY(1)                                          799
         AG_Z = TOFF_BLEMPOSZ1                                            799
         AG_TITLE = 'BLEM'                                                799
         AG_EXNAME = 'POSITION'                                           799
         AG_PARLIST = 'X_Y_Z'                                             799
         CALL AXPOSITION                                                  799
C    *                                                                    799
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(2) Z=TOF  799
C    F_BLEMPOSZ1 <===                                                     799
         CALL AGSRESET                                                    800
         AG_X = TOFF_BLEMPOSX                                             800
         AG_Y = TOFF_BLEMPOSY(2)                                          800
         AG_Z = TOFF_BLEMPOSZ1                                            800
         AG_TITLE = 'BLEM'                                                800
         AG_EXNAME = 'POSITION'                                           800
         AG_PARLIST = 'X_Y_Z'                                             800
         CALL AXPOSITION                                                  800
C    *                                                                    800
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(3) Z=TOF  800
C    F_BLEMPOSZ1 <===                                                     800
         CALL AGSRESET                                                    801
         AG_X = TOFF_BLEMPOSX                                             801
         AG_Y = TOFF_BLEMPOSY(3)                                          801
         AG_Z = TOFF_BLEMPOSZ1                                            801
         AG_TITLE = 'BLEM'                                                801
         AG_EXNAME = 'POSITION'                                           801
         AG_PARLIST = 'X_Y_Z'                                             801
         CALL AXPOSITION                                                  801
C    *                                                                    801
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(4) Z=TOF  801
C    F_BLEMPOSZ1 <===                                                     801
         CALL AGSRESET                                                    802
         AG_X = TOFF_BLEMPOSX                                             802
         AG_Y = TOFF_BLEMPOSY(4)                                          802
         AG_Z = TOFF_BLEMPOSZ1                                            802
         AG_TITLE = 'BLEM'                                                802
         AG_EXNAME = 'POSITION'                                           802
         AG_PARLIST = 'X_Y_Z'                                             802
         CALL AXPOSITION                                                  802
C    *                                                                    802
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(5) Z=TOF  802
C    F_BLEMPOSZ1 <===                                                     802
         CALL AGSRESET                                                    803
         AG_X = TOFF_BLEMPOSX                                             803
         AG_Y = TOFF_BLEMPOSY(5)                                          803
         AG_Z = TOFF_BLEMPOSZ1                                            803
         AG_TITLE = 'BLEM'                                                803
         AG_EXNAME = 'POSITION'                                           803
         AG_PARLIST = 'X_Y_Z'                                             803
         CALL AXPOSITION                                                  803
C    *                                                                    803
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(6) Z=TOF  803
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          803
         CALL AGSRESET                                                    804
         AG_X = TOFF_BLEMPOSX                                             804
         AG_Y = TOFF_BLEMPOSY(6)                                          804
         AG_Z = TOFF_BLEMPOSZ2                                            804
         AG_ALPHAN=AG_ALPHAN+1                                            804
         AG_ALPHAK(AG_ALPHAN)=1                                           804
         AG_ALPHAL(AG_ALPHAN)=180                                         804
         AG_TITLE = 'BLEM'                                                804
         AG_EXNAME = 'POSITION'                                           804
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      804
         CALL AXPOSITION                                                  804
C    *                                                                    804
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(7) Z=TOF  804
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          804
         CALL AGSRESET                                                    805
         AG_X = TOFF_BLEMPOSX                                             805
         AG_Y = TOFF_BLEMPOSY(7)                                          805
         AG_Z = TOFF_BLEMPOSZ2                                            805
         AG_ALPHAN=AG_ALPHAN+1                                            805
         AG_ALPHAK(AG_ALPHAN)=1                                           805
         AG_ALPHAL(AG_ALPHAN)=180                                         805
         AG_TITLE = 'BLEM'                                                805
         AG_EXNAME = 'POSITION'                                           805
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      805
         CALL AXPOSITION                                                  805
C    *                                                                    805
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(8) Z=TOF  805
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          805
         CALL AGSRESET                                                    806
         AG_X = TOFF_BLEMPOSX                                             806
         AG_Y = TOFF_BLEMPOSY(8)                                          806
         AG_Z = TOFF_BLEMPOSZ2                                            806
         AG_ALPHAN=AG_ALPHAN+1                                            806
         AG_ALPHAK(AG_ALPHAN)=1                                           806
         AG_ALPHAL(AG_ALPHAN)=180                                         806
         AG_TITLE = 'BLEM'                                                806
         AG_EXNAME = 'POSITION'                                           806
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      806
         CALL AXPOSITION                                                  806
C    *                                                                    806
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(9) Z=TOF  806
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          806
         CALL AGSRESET                                                    807
         AG_X = TOFF_BLEMPOSX                                             807
         AG_Y = TOFF_BLEMPOSY(9)                                          807
         AG_Z = TOFF_BLEMPOSZ2                                            807
         AG_ALPHAN=AG_ALPHAN+1                                            807
         AG_ALPHAK(AG_ALPHAN)=1                                           807
         AG_ALPHAL(AG_ALPHAN)=180                                         807
         AG_TITLE = 'BLEM'                                                807
         AG_EXNAME = 'POSITION'                                           807
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      807
         CALL AXPOSITION                                                  807
C    *                                                                    807
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(10) Z=TO  807
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         807
         CALL AGSRESET                                                    808
         AG_X = TOFF_BLEMPOSX                                             808
         AG_Y = TOFF_BLEMPOSY(10)                                         808
         AG_Z = TOFF_BLEMPOSZ2                                            808
         AG_ALPHAN=AG_ALPHAN+1                                            808
         AG_ALPHAK(AG_ALPHAN)=1                                           808
         AG_ALPHAL(AG_ALPHAN)=180                                         808
         AG_TITLE = 'BLEM'                                                808
         AG_EXNAME = 'POSITION'                                           808
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      808
         CALL AXPOSITION                                                  808
C    *                                                                    808
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(11) Z=TO  808
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         808
         CALL AGSRESET                                                    809
         AG_X = TOFF_BLEMPOSX                                             809
         AG_Y = TOFF_BLEMPOSY(11)                                         809
         AG_Z = TOFF_BLEMPOSZ2                                            809
         AG_ALPHAN=AG_ALPHAN+1                                            809
         AG_ALPHAK(AG_ALPHAN)=1                                           809
         AG_ALPHAL(AG_ALPHAN)=180                                         809
         AG_TITLE = 'BLEM'                                                809
         AG_EXNAME = 'POSITION'                                           809
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      809
         CALL AXPOSITION                                                  809
C    *                                                                    809
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(12) Z=TO  809
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         809
         CALL AGSRESET                                                    810
         AG_X = TOFF_BLEMPOSX                                             810
         AG_Y = TOFF_BLEMPOSY(12)                                         810
         AG_Z = TOFF_BLEMPOSZ2                                            810
         AG_ALPHAN=AG_ALPHAN+1                                            810
         AG_ALPHAK(AG_ALPHAN)=1                                           810
         AG_ALPHAL(AG_ALPHAN)=180                                         810
         AG_TITLE = 'BLEM'                                                810
         AG_EXNAME = 'POSITION'                                           810
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      810
         CALL AXPOSITION                                                  810
C    *                                                                    810
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(13) Z=TO  810
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         810
         CALL AGSRESET                                                    811
         AG_X = TOFF_BLEMPOSX                                             811
         AG_Y = TOFF_BLEMPOSY(13)                                         811
         AG_Z = TOFF_BLEMPOSZ2                                            811
         AG_ALPHAN=AG_ALPHAN+1                                            811
         AG_ALPHAK(AG_ALPHAN)=1                                           811
         AG_ALPHAL(AG_ALPHAN)=180                                         811
         AG_TITLE = 'BLEM'                                                811
         AG_EXNAME = 'POSITION'                                           811
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      811
         CALL AXPOSITION                                                  811
C    *                                                                    811
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(14) Z=TO  811
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         811
         CALL AGSRESET                                                    812
         AG_X = TOFF_BLEMPOSX                                             812
         AG_Y = TOFF_BLEMPOSY(14)                                         812
         AG_Z = TOFF_BLEMPOSZ2                                            812
         AG_ALPHAN=AG_ALPHAN+1                                            812
         AG_ALPHAK(AG_ALPHAN)=1                                           812
         AG_ALPHAL(AG_ALPHAN)=180                                         812
         AG_TITLE = 'BLEM'                                                812
         AG_EXNAME = 'POSITION'                                           812
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      812
         CALL AXPOSITION                                                  812
C    *                                                                    812
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(15) Z=TO  812
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         812
         CALL AGSRESET                                                    813
         AG_X = TOFF_BLEMPOSX                                             813
         AG_Y = TOFF_BLEMPOSY(15)                                         813
         AG_Z = TOFF_BLEMPOSZ2                                            813
         AG_ALPHAN=AG_ALPHAN+1                                            813
         AG_ALPHAK(AG_ALPHAN)=1                                           813
         AG_ALPHAL(AG_ALPHAN)=180                                         813
         AG_TITLE = 'BLEM'                                                813
         AG_EXNAME = 'POSITION'                                           813
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      813
         CALL AXPOSITION                                                  813
C    *                                                                    813
5431  CALL AGSPOPB                                                        814
      GO TO BLOCK_BFEE, (5430 ,5440)                                      814
C End of Local Procedure                                                  814
C ----------------------------------------------------------------------  815
C ------------ BLEM                                                       815
C Begin Local Procedure BLEM                                              816
C BLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              816
5730  CONTINUE                                                            816
         AG_TITLE='BLEM'                                                  816
         AG_MODULE=MODULE                                                 816
         AG_MARK='V00'                                                    816
         CALL AGSPUSH                                                     816
         AG_MODULE=MODULE                                                 816
C    *                                                                    816
C    Geant ATTRIBUTE : BLEM ===> SEEN=0 COLO=3 <===                       817
         CALL AGSRESET                                                    817
         AG_SEEN = 0                                                      817
         AG_COLO = 3                                                      817
         AG_TITLE = 'BLEM'                                                817
         AG_EXNAME = 'ATTRIBUTE'                                          817
         AG_PARLIST = 'SEEN_COLO'                                         817
         CALL AXATTRIBUTE                                                 817
C    *                                                                    817
C    Geant SHAPE : BOX ===> DX=TOFF_BLEMLENX/2 DY=TOFF_BLEMLENY/2, DZ=TO  817
C    FF_BLEMLENZ/2 <===                                                   817
         CALL AGSRESET                                                    819
         AG_DX = TOFF_BLEMLENX/2                                          819
         AG_DY = TOFF_BLEMLENY/2                                          819
         AG_DZ = TOFF_BLEMLENZ/2                                          819
         AG_TITLE = 'BOX'                                                 819
         AG_EXNAME = 'SHAPE'                                              819
         AG_PARLIST = 'DX_DY_DZ'                                          819
         CALL AXSHAPE                                                     819
C    *                                                                    819
C    Create BPIP :                                                        820
C    Call To Local Procedure BPIP                                         820
         ASSIGN 5760 TO BLOCK_BPIP                                        820
         GO TO  5750                                                      820
5760     CONTINUE                                                         820
C    *                                                                    820
C    Geant POSITION : BPIP ===> X=TOFF_BPIPPOSX Y=0 Z=TOFF_BPIPPOSZ RMIN  822
C    =TOFF_BPIPRMIN RMAX=TOFF_BPIPRMAX DZ=TOFF_BPIPLENZ/2 <===            822
         CALL AGSRESET                                                    822
         AG_X = TOFF_BPIPPOSX                                             822
         AG_Y = 0                                                         822
         AG_Z = TOFF_BPIPPOSZ                                             822
         AG_RMIN = TOFF_BPIPRMIN                                          822
         AG_RMAX = TOFF_BPIPRMAX                                          822
         AG_DZ = TOFF_BPIPLENZ/2                                          822
         AG_TITLE = 'BPIP'                                                822
         AG_EXNAME = 'POSITION'                                           822
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                822
         CALL AXPOSITION                                                  822
C    *                                                                    822
5731  CALL AGSPOPB                                                        823
      GO TO BLOCK_BLEM, (5730 ,5740)                                      823
C End of Local Procedure                                                  823
C ----------------------------------------------------------------------  824
C ------------ BCOO                                                       824
C Begin Local Procedure BCOO                                              825
C BCOO ARE THE COOLING RAILS/LOOPS                                        825
5410  CONTINUE                                                            825
         AG_TITLE='BCOO'                                                  825
         AG_MODULE=MODULE                                                 825
         AG_MARK='V00'                                                    825
         CALL AGSPUSH                                                     825
         AG_MODULE=MODULE                                                 825
C    *                                                                    825
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       826
         CALL AGSRESET                                                    826
         AG_SEEN = 0                                                      826
         AG_COLO = 2                                                      826
         AG_TITLE = 'BCOO'                                                826
         AG_EXNAME = 'ATTRIBUTE'                                          826
         AG_PARLIST = 'SEEN_COLO'                                         826
         CALL AXATTRIBUTE                                                 826
C    *                                                                    826
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           826
         CALL AGSRESET                                                    827
         AG_DX = 0                                                        827
         AG_DY = 0                                                        827
         AG_DZ = 0                                                        827
         AG_TITLE = 'BOX'                                                 827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'DX_DY_DZ'                                          827
         CALL AXSHAPE                                                     827
C    *                                                                    827
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               827
         AG_DX = TOFF_RAILTHCK/2                                          828
         AG_DY = TOFF_RAILWID/2                                           828
C    Create BRAI :                                                        828
C    Call To Local Procedure BRAI                                         828
         ASSIGN 5780 TO BLOCK_BRAI                                        828
         GO TO  5770                                                      828
5780     CONTINUE                                                         828
C    *                                                                    828
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  830
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  830
         CALL AGSRESET                                                    830
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                830
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                830
         AG_KONLY = 'MANY'                                                830
         AG_TITLE = 'BRAI'                                                830
         AG_EXNAME = 'POSITION'                                           830
         AG_PARLIST = 'X_Y_KONLY'                                         830
         CALL AXPOSITION                                                  830
C    *                                                                    830
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  830
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  830
C    ='MANY' <===                                                         830
         CALL AGSRESET                                                    832
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               832
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               832
         AG_ALPHAN=AG_ALPHAN+1                                            832
         AG_ALPHAK(AG_ALPHAN)=3                                           832
         AG_ALPHAL(AG_ALPHAN)=90                                          832
         AG_KONLY = 'MANY'                                                832
         AG_TITLE = 'BRAI'                                                832
         AG_EXNAME = 'POSITION'                                           832
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  832
         CALL AXPOSITION                                                  832
C    *                                                                    832
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  832
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 832
         CALL AGSRESET                                                    834
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                834
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               834
         AG_KONLY = 'MANY'                                                834
         AG_TITLE = 'BRAI'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'X_Y_KONLY'                                         834
         CALL AXPOSITION                                                  834
C    *                                                                    834
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  834
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  834
C    Y='MANY' <===                                                        834
         CALL AGSRESET                                                    836
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               836
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              836
         AG_ALPHAN=AG_ALPHAN+1                                            836
         AG_ALPHAK(AG_ALPHAN)=3                                           836
         AG_ALPHAL(AG_ALPHAN)=90                                          836
         AG_KONLY = 'MANY'                                                836
         AG_TITLE = 'BRAI'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  836
         CALL AXPOSITION                                                  836
C    *                                                                    836
         AG_RMAX = TOFF_COOLOUTR                                          836
         AG_RMIN = TOFF_COOLINNR                                          837
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               837
C    Create BPIP :                                                        837
C    Call To Local Procedure BPIP                                         837
         ASSIGN 5790 TO BLOCK_BPIP                                        837
         GO TO  5750                                                      837
5790     CONTINUE                                                         837
C    *                                                                    837
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  839
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   839
C    KONLY='MANY' <===                                                    839
         CALL AGSRESET                                                    839
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            839
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   839
         AG_KONLY = 'MANY'                                                839
         AG_TITLE = 'BPIP'                                                839
         AG_EXNAME = 'POSITION'                                           839
         AG_PARLIST = 'X_Y_KONLY'                                         839
         CALL AXPOSITION                                                  839
C    *                                                                    839
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  839
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  839
C     KONLY='MANY' <===                                                   839
         CALL AGSRESET                                                    841
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            841
         AG_Y = -(TRAY_WIDTH/                                             841
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      841
         AG_KONLY = 'MANY'                                                841
         AG_TITLE = 'BPIP'                                                841
         AG_EXNAME = 'POSITION'                                           841
         AG_PARLIST = 'X_Y_KONLY'                                         841
         CALL AXPOSITION                                                  841
C    *                                                                    841
         AG_RMAX = TOFF_COOLOUTR                                          841
         AG_RMIN = TOFF_COOLINNR                                          842
         AG_DZ = TRAY_WIDTH/                                              842
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    842
C    Create BPIQ :                                                        842
C    Call To Local Procedure BPIQ                                         842
         ASSIGN 5810 TO BLOCK_BPIQ                                        842
         GO TO  5800                                                      842
5810     CONTINUE                                                         842
C    *                                                                    842
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  845
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  845
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     845
         CALL AGSRESET                                                    845
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            845
         AG_Y = 0.0                                                       845
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    845
         AG_ALPHAN=AG_ALPHAN+1                                            845
         AG_ALPHAK(AG_ALPHAN)=1                                           845
         AG_ALPHAL(AG_ALPHAN)=90                                          845
         AG_KONLY = 'MANY'                                                845
         AG_TITLE = 'BPIQ'                                                845
         AG_EXNAME = 'POSITION'                                           845
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                845
         CALL AXPOSITION                                                  845
C    *                                                                    845
         AG_RMAX = TOFF_COOLINNR                                          845
         AG_RMIN = 0                                                      846
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               846
C    Create BWAT :                                                        846
C    Call To Local Procedure BWAT                                         846
         ASSIGN 5830 TO BLOCK_BWAT                                        846
         GO TO  5820                                                      846
5830     CONTINUE                                                         846
C    *                                                                    846
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  848
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   848
C    <===                                                                 848
         CALL AGSRESET                                                    848
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            848
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   848
         AG_TITLE = 'BWAT'                                                848
         AG_EXNAME = 'POSITION'                                           848
         AG_PARLIST = 'X_Y'                                               848
         CALL AXPOSITION                                                  848
C    *                                                                    848
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  848
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  848
C     <===                                                                848
         CALL AGSRESET                                                    850
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            850
         AG_Y = -(TRAY_WIDTH/                                             850
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      850
         AG_TITLE = 'BWAT'                                                850
         AG_EXNAME = 'POSITION'                                           850
         AG_PARLIST = 'X_Y'                                               850
         CALL AXPOSITION                                                  850
C    *                                                                    850
         AG_RMAX = TOFF_COOLINNR                                          850
         AG_RMIN = 0                                                      851
         AG_DZ = TRAY_WIDTH/                                              851
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    851
C    Create BWAT :                                                        851
C    Call To Local Procedure BWAT                                         851
         ASSIGN 5840 TO BLOCK_BWAT                                        851
         GO TO  5820                                                      851
5840     CONTINUE                                                         851
C    *                                                                    851
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  853
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  853
C    OUTR ALPHAX=90 <===                                                  853
         CALL AGSRESET                                                    853
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            853
         AG_Y = 0.0                                                       853
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    853
         AG_ALPHAN=AG_ALPHAN+1                                            853
         AG_ALPHAK(AG_ALPHAN)=1                                           853
         AG_ALPHAL(AG_ALPHAN)=90                                          853
         AG_TITLE = 'BWAT'                                                853
         AG_EXNAME = 'POSITION'                                           853
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      853
         CALL AXPOSITION                                                  853
C    *                                                                    853
5411  CALL AGSPOPB                                                        854
      GO TO BLOCK_BCOO, (5410 ,5420)                                      854
C End of Local Procedure                                                  854
C ----------------------------------------------------------------------  855
C ------------ BRAI                                                       855
C Begin Local Procedure BRAI                                              856
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   856
5770  CONTINUE                                                            856
         AG_TITLE='BRAI'                                                  856
         AG_MODULE=MODULE                                                 856
         AG_MARK='V00'                                                    856
         CALL AGSPUSH                                                     856
         AG_MODULE=MODULE                                                 856
C    *                                                                    856
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       857
         CALL AGSRESET                                                    857
         AG_SEEN = 0                                                      857
         AG_COLO = 7                                                      857
         AG_TITLE = 'BRAI'                                                857
         AG_EXNAME = 'ATTRIBUTE'                                          857
         AG_PARLIST = 'SEEN_COLO'                                         857
         CALL AXATTRIBUTE                                                 857
C    *                                                                    857
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        857
         CALL AGSRESET                                                    858
         AG_TITLE = 'ALUMINIUM'                                           858
         AG_EXNAME = 'MATERIAL'                                           858
         AG_PARLIST = 'NONE'                                              858
         CALL AXMATERIAL                                                  858
C    *                                                                    858
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     858
         CALL AGSRESET                                                    859
         AG_DX = 0.0                                                      859
         AG_DY = 0.0                                                      859
         AG_DZ = 0.0                                                      859
         AG_TITLE = 'BOX'                                                 859
         AG_EXNAME = 'SHAPE'                                              859
         AG_PARLIST = 'DX_DY_DZ'                                          859
         CALL AXSHAPE                                                     859
C    *                                                                    859
5771  CALL AGSPOPB                                                        860
      GO TO BLOCK_BRAI, (5770 ,5780)                                      860
C End of Local Procedure                                                  860
C ----------------------------------------------------------------------  861
C ------------ BPIP                                                       861
C Begin Local Procedure BPIP                                              862
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              862
5750  CONTINUE                                                            862
         AG_TITLE='BPIP'                                                  862
         AG_MODULE=MODULE                                                 862
         AG_MARK='V00'                                                    862
         CALL AGSPUSH                                                     862
         AG_MODULE=MODULE                                                 862
C    *                                                                    862
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       863
         CALL AGSRESET                                                    863
         AG_SEEN = 0                                                      863
         AG_COLO = 7                                                      863
         AG_TITLE = 'BPIP'                                                863
         AG_EXNAME = 'ATTRIBUTE'                                          863
         AG_PARLIST = 'SEEN_COLO'                                         863
         CALL AXATTRIBUTE                                                 863
C    *                                                                    863
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        863
         CALL AGSRESET                                                    864
         AG_TITLE = 'ALUMINIUM'                                           864
         AG_EXNAME = 'MATERIAL'                                           864
         AG_PARLIST = 'NONE'                                              864
         CALL AXMATERIAL                                                  864
C    *                                                                    864
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      864
         CALL AGSRESET                                                    865
         AG_RMIN = 0                                                      865
         AG_RMAX = 0                                                      865
         AG_DZ = 0                                                        865
         AG_TITLE = 'TUBE'                                                865
         AG_EXNAME = 'SHAPE'                                              865
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      865
         CALL AXSHAPE                                                     865
C    *                                                                    865
5751  CALL AGSPOPB                                                        866
      GO TO BLOCK_BPIP, (5750 ,5760,5790)                                 866
C End of Local Procedure                                                  866
C ----------------------------------------------------------------------  867
C ------------ BPIQ                                                       867
C Begin Local Procedure BPIQ                                              868
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             868
5800  CONTINUE                                                            868
         AG_TITLE='BPIQ'                                                  868
         AG_MODULE=MODULE                                                 868
         AG_MARK='V00'                                                    868
         CALL AGSPUSH                                                     868
         AG_MODULE=MODULE                                                 868
C    *                                                                    868
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       869
         CALL AGSRESET                                                    869
         AG_SEEN = 0                                                      869
         AG_COLO = 7                                                      869
         AG_TITLE = 'BPIQ'                                                869
         AG_EXNAME = 'ATTRIBUTE'                                          869
         AG_PARLIST = 'SEEN_COLO'                                         869
         CALL AXATTRIBUTE                                                 869
C    *                                                                    869
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        869
         CALL AGSRESET                                                    870
         AG_TITLE = 'ALUMINIUM'                                           870
         AG_EXNAME = 'MATERIAL'                                           870
         AG_PARLIST = 'NONE'                                              870
         CALL AXMATERIAL                                                  870
C    *                                                                    870
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      870
         CALL AGSRESET                                                    871
         AG_RMIN = 0                                                      871
         AG_RMAX = 0                                                      871
         AG_DZ = 0                                                        871
         AG_TITLE = 'TUBE'                                                871
         AG_EXNAME = 'SHAPE'                                              871
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      871
         CALL AXSHAPE                                                     871
C    *                                                                    871
5801  CALL AGSPOPB                                                        872
      GO TO BLOCK_BPIQ, (5800 ,5810)                                      872
C End of Local Procedure                                                  872
C ----------------------------------------------------------------------  873
C ------------ BUND                                                       873
C Begin Local Procedure BUND                                              874
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          874
5170  CONTINUE                                                            874
         AG_TITLE='BUND'                                                  874
         AG_MODULE=MODULE                                                 874
         AG_MARK='V00'                                                    874
         CALL AGSPUSH                                                     874
         AG_MODULE=MODULE                                                 874
C    *                                                                    874
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              875
         CALL AGSRESET                                                    875
         AG_SEEN = 0                                                      875
         AG_COLO = 1                                                      875
         AG_SERIAL = 0                                                    875
         AG_TITLE = 'BUND'                                                875
         AG_EXNAME = 'ATTRIBUTE'                                          875
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  875
         CALL AXATTRIBUTE                                                 875
C    *                                                                    875
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  875
C    SUPLEN/2 <===                                                        875
         CALL AGSRESET                                                    877
         AG_DX = TRAY_SUPFULLH/2                                          877
         AG_DY = TRAY_WIDTH/2                                             877
         AG_DZ = TRAY_SUPLEN/2                                            877
         AG_TITLE = 'BOX'                                                 877
         AG_EXNAME = 'SHAPE'                                              877
         AG_PARLIST = 'DX_DY_DZ'                                          877
         CALL AXSHAPE                                                     877
C    *                                                                    877
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        877
         CALL AGSRESET                                                    878
         AG_TITLE = 'ALUMINIUM'                                           878
         AG_EXNAME = 'MATERIAL'                                           878
         AG_PARLIST = 'NONE'                                              878
         CALL AXMATERIAL                                                  878
C    *                                                                    878
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           878
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             880
C    Create BTFT :                                                        881
C    Call To Local Procedure BTFT                                         881
         ASSIGN 5860 TO BLOCK_BTFT                                        881
         GO TO  5850                                                      881
5860     CONTINUE                                                         881
C    *                                                                    881
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  882
C    _FOOT1LEN/2 <===                                                     882
         CALL AGSRESET                                                    882
         AG_X = XPOS                                                      882
         AG_Y = -YPOS                                                     882
         AG_DX = TRAY_FOOTTHK/2                                           882
         AG_DY = TRAY_FOOT1LEN/2                                          882
         AG_TITLE = 'BTFT'                                                882
         AG_EXNAME = 'POSITION'                                           882
         AG_PARLIST = 'X_Y_DX_DY'                                         882
         CALL AXPOSITION                                                  882
C    *                                                                    882
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  882
C    _FOOT1LEN/2 <===                                                     882
         CALL AGSRESET                                                    883
         AG_X = XPOS                                                      883
         AG_Y = +YPOS                                                     883
         AG_DX = TRAY_FOOTTHK/2                                           883
         AG_DY = TRAY_FOOT1LEN/2                                          883
         AG_TITLE = 'BTFT'                                                883
         AG_EXNAME = 'POSITION'                                           883
         AG_PARLIST = 'X_Y_DX_DY'                                         883
         CALL AXPOSITION                                                  883
C    *                                                                    883
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          883
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   885
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  886
C    Y_FOOTTHK/2 <===                                                     886
         CALL AGSRESET                                                    886
         AG_X = XPOS                                                      886
         AG_Y = -YPOS                                                     886
         AG_DX = TRAY_FOOT2THK/2                                          886
         AG_DY = TRAY_FOOTTHK/2                                           886
         AG_TITLE = 'BTFT'                                                886
         AG_EXNAME = 'POSITION'                                           886
         AG_PARLIST = 'X_Y_DX_DY'                                         886
         CALL AXPOSITION                                                  886
C    *                                                                    886
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  886
C    Y_FOOTTHK/2 <===                                                     886
         CALL AGSRESET                                                    887
         AG_X = XPOS                                                      887
         AG_Y = +YPOS                                                     887
         AG_DX = TRAY_FOOT2THK/2                                          887
         AG_DY = TRAY_FOOTTHK/2                                           887
         AG_TITLE = 'BTFT'                                                887
         AG_EXNAME = 'POSITION'                                           887
         AG_PARLIST = 'X_Y_DX_DY'                                         887
         CALL AXPOSITION                                                  887
C    *                                                                    887
         XPOS =(TRAY_SUPFULLH + TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           887
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   889
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  890
C    _FOOT3LEN/2 <===                                                     890
         CALL AGSRESET                                                    890
         AG_X = XPOS                                                      890
         AG_Y = -YPOS                                                     890
         AG_DX = TRAY_FOOTTHK/2                                           890
         AG_DY = TRAY_FOOT3LEN/2                                          890
         AG_TITLE = 'BTFT'                                                890
         AG_EXNAME = 'POSITION'                                           890
         AG_PARLIST = 'X_Y_DX_DY'                                         890
         CALL AXPOSITION                                                  890
C    *                                                                    890
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  890
C    _FOOT3LEN/2 <===                                                     890
         CALL AGSRESET                                                    891
         AG_X = XPOS                                                      891
         AG_Y = +YPOS                                                     891
         AG_DX = TRAY_FOOTTHK/2                                           891
         AG_DY = TRAY_FOOT3LEN/2                                          891
         AG_TITLE = 'BTFT'                                                891
         AG_EXNAME = 'POSITION'                                           891
         AG_PARLIST = 'X_Y_DX_DY'                                         891
         CALL AXPOSITION                                                  891
C    *                                                                    891
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              891
     *   TAN(60*DEGRAD)                                                   891
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           893
     *   SUPPORT_AILE_WIDTH                                               893
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        894
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               895
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          896
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        897
         CALL AGSRESET                                                    897
         AG_TITLE = 'ALUMINIUM'                                           897
         AG_EXNAME = 'MATERIAL'                                           897
         AG_PARLIST = 'NONE'                                              897
         CALL AXMATERIAL                                                  897
C    *                                                                    897
C    Create BASE :                                                        898
C    Call To Local Procedure BASE                                         898
         ASSIGN 5880 TO BLOCK_BASE                                        898
         GO TO  5870                                                      898
5880     CONTINUE                                                         898
C    *                                                                    898
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   899
         CALL AGSRESET                                                    899
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          899
         AG_TITLE = 'BASE'                                                899
         AG_EXNAME = 'POSITION'                                           899
         AG_PARLIST = 'X'                                                 899
         CALL AXPOSITION                                                  899
C    *                                                                    899
C    Create BARM :                                                        900
C    Call To Local Procedure BARM                                         900
         ASSIGN 5900 TO BLOCK_BARM                                        900
         GO TO  5890                                                      900
5900     CONTINUE                                                         900
C    *                                                                    900
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  901
C    ===                                                                  901
         CALL AGSRESET                                                    901
         AG_X = SUPPORT_ARM_XPOS                                          901
         AG_Y = -SUPPORT_ARM_YPOS                                         901
         AG_TITLE = 'BARM'                                                901
         AG_EXNAME = 'POSITION'                                           901
         AG_PARLIST = 'X_Y'                                               901
         CALL AXPOSITION                                                  901
C    *                                                                    901
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  901
C    ===                                                                  901
         CALL AGSRESET                                                    902
         AG_X = SUPPORT_ARM_XPOS                                          902
         AG_Y = +SUPPORT_ARM_YPOS                                         902
         AG_TITLE = 'BARM'                                                902
         AG_EXNAME = 'POSITION'                                           902
         AG_PARLIST = 'X_Y'                                               902
         CALL AXPOSITION                                                  902
C    *                                                                    902
C    Create BANG :                                                        903
C    Call To Local Procedure BANG                                         903
         ASSIGN 5920 TO BLOCK_BANG                                        903
         GO TO  5910                                                      903
5920     CONTINUE                                                         903
C    *                                                                    903
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 904
         CALL AGSRESET                                                    904
         AG_Y = -SUPPORT_AILE_YPOS                                        904
         AG_TITLE = 'BANG'                                                904
         AG_EXNAME = 'POSITION'                                           904
         AG_PARLIST = 'Y'                                                 904
         CALL AXPOSITION                                                  904
C    *                                                                    904
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      904
         CALL AGSRESET                                                    905
         AG_Y = +SUPPORT_AILE_YPOS                                        905
         AG_THETAX = 270                                                  905
         AG_TITLE = 'BANG'                                                905
         AG_EXNAME = 'POSITION'                                           905
         AG_PARLIST = 'Y_THETAX'                                          905
         CALL AXPOSITION                                                  905
C    *                                                                    905
C    Create BCOV :                                                        906
C    Call To Local Procedure BCOV                                         906
         ASSIGN 5940 TO BLOCK_BCOV                                        906
         GO TO  5930                                                      906
5940     CONTINUE                                                         906
C    *                                                                    906
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     907
         CALL AGSRESET                                                    907
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            907
         AG_TITLE = 'BCOV'                                                907
         AG_EXNAME = 'POSITION'                                           907
         AG_PARLIST = 'X'                                                 907
         CALL AXPOSITION                                                  907
C    *                                                                    907
5171  CALL AGSPOPB                                                        908
      GO TO BLOCK_BUND, (5170 ,5180)                                      908
C End of Local Procedure                                                  908
C ----------------------------------------------------------------------  909
C ------------ BTFT                                                       909
C Begin Local Procedure BTFT                                              910
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       910
5850  CONTINUE                                                            910
         AG_TITLE='BTFT'                                                  910
         AG_MODULE=MODULE                                                 910
         AG_MARK='V00'                                                    910
         CALL AGSPUSH                                                     910
         AG_MODULE=MODULE                                                 910
C    *                                                                    910
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       911
         CALL AGSRESET                                                    911
         AG_SEEN = 0                                                      911
         AG_COLO = 2                                                      911
         AG_TITLE = 'BTFT'                                                911
         AG_EXNAME = 'ATTRIBUTE'                                          911
         AG_PARLIST = 'SEEN_COLO'                                         911
         CALL AXATTRIBUTE                                                 911
C    *                                                                    911
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     911
         CALL AGSRESET                                                    912
         AG_DX = 0.0                                                      912
         AG_DY = 0.0                                                      912
         AG_DZ = 0.0                                                      912
         AG_TITLE = 'BOX'                                                 912
         AG_EXNAME = 'SHAPE'                                              912
         AG_PARLIST = 'DX_DY_DZ'                                          912
         CALL AXSHAPE                                                     912
C    *                                                                    912
5851  CALL AGSPOPB                                                        913
      GO TO BLOCK_BTFT, (5850 ,5860)                                      913
C End of Local Procedure                                                  913
C ----------------------------------------------------------------------  914
C ------------ BARM                                                       914
C Begin Local Procedure BARM                                              915
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              915
5890  CONTINUE                                                            915
         AG_TITLE='BARM'                                                  915
         AG_MODULE=MODULE                                                 915
         AG_MARK='V00'                                                    915
         CALL AGSPUSH                                                     915
         AG_MODULE=MODULE                                                 915
C    *                                                                    915
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       916
         CALL AGSRESET                                                    916
         AG_SEEN = 0                                                      916
         AG_COLO = 2                                                      916
         AG_TITLE = 'BARM'                                                916
         AG_EXNAME = 'ATTRIBUTE'                                          916
         AG_PARLIST = 'SEEN_COLO'                                         916
         CALL AXATTRIBUTE                                                 916
C    *                                                                    916
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  916
C    =                                                                    916
         CALL AGSRESET                                                    917
         AG_DX = TRAY_SUPARMT/2                                           917
         AG_DY = SUPPORT_ARM_WIDTH/2                                      917
         AG_TITLE = 'BOX'                                                 917
         AG_EXNAME = 'SHAPE'                                              917
         AG_PARLIST = 'DX_DY'                                             917
         CALL AXSHAPE                                                     917
C    *                                                                    917
5891  CALL AGSPOPB                                                        918
      GO TO BLOCK_BARM, (5890 ,5900)                                      918
C End of Local Procedure                                                  918
C ----------------------------------------------------------------------  919
C ------------ BANG                                                       919
C Begin Local Procedure BANG                                              920
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                920
5910  CONTINUE                                                            920
         AG_TITLE='BANG'                                                  920
         AG_MODULE=MODULE                                                 920
         AG_MARK='V00'                                                    920
         CALL AGSPUSH                                                     920
         AG_MODULE=MODULE                                                 920
C    *                                                                    920
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       921
         CALL AGSRESET                                                    921
         AG_SEEN = 0                                                      921
         AG_COLO = 2                                                      921
         AG_TITLE = 'BANG'                                                921
         AG_EXNAME = 'ATTRIBUTE'                                          921
         AG_PARLIST = 'SEEN_COLO'                                         921
         CALL AXATTRIBUTE                                                 921
C    *                                                                    921
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   921
C    ALPH=-60 THET=0 PHI=0 <===                                           921
         CALL AGSRESET                                                    923
         AG_DX = TRAY_SUPARMT/2                                           923
         AG_DY = SUPPORT_AILE_WIDTH/2                                     923
         AG_ALPH = -60                                                    923
         AG_THET = 0                                                      923
         AG_PHI = 0                                                       923
         AG_TITLE = 'PARA'                                                923
         AG_EXNAME = 'SHAPE'                                              923
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               923
         CALL AXSHAPE                                                     923
C    *                                                                    923
5911  CALL AGSPOPB                                                        924
      GO TO BLOCK_BANG, (5910 ,5920)                                      924
C End of Local Procedure                                                  924
C ----------------------------------------------------------------------  925
C ------------ BASE                                                       925
C Begin Local Procedure BASE                                              926
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               926
5870  CONTINUE                                                            926
         AG_TITLE='BASE'                                                  926
         AG_MODULE=MODULE                                                 926
         AG_MARK='V00'                                                    926
         CALL AGSPUSH                                                     926
         AG_MODULE=MODULE                                                 926
C    *                                                                    926
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       927
         CALL AGSRESET                                                    927
         AG_SEEN = 0                                                      927
         AG_COLO = 2                                                      927
         AG_TITLE = 'BASE'                                                927
         AG_EXNAME = 'ATTRIBUTE'                                          927
         AG_PARLIST = 'SEEN_COLO'                                         927
         CALL AXATTRIBUTE                                                 927
C    *                                                                    927
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    927
         CALL AGSRESET                                                    928
         AG_DX = TRAY_SUPBASET/2                                          928
         AG_DY = TRAY_SUPBASEW/2                                          928
         AG_TITLE = 'BOX'                                                 928
         AG_EXNAME = 'SHAPE'                                              928
         AG_PARLIST = 'DX_DY'                                             928
         CALL AXSHAPE                                                     928
C    *                                                                    928
5871  CALL AGSPOPB                                                        929
      GO TO BLOCK_BASE, (5870 ,5880)                                      929
C End of Local Procedure                                                  929
C ----------------------------------------------------------------------  930
C ------------ BCOV                                                       930
C Begin Local Procedure BCOV                                              931
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     931
5930  CONTINUE                                                            931
         AG_TITLE='BCOV'                                                  931
         AG_MODULE=MODULE                                                 931
         AG_MARK='V00'                                                    931
         CALL AGSPUSH                                                     931
         AG_MODULE=MODULE                                                 931
C    *                                                                    931
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       932
         CALL AGSRESET                                                    932
         AG_SEEN = 0                                                      932
         AG_COLO = 2                                                      932
         AG_TITLE = 'BCOV'                                                932
         AG_EXNAME = 'ATTRIBUTE'                                          932
         AG_PARLIST = 'SEEN_COLO'                                         932
         CALL AXATTRIBUTE                                                 932
C    *                                                                    932
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               932
         CALL AGSRESET                                                    933
         AG_RMIN = 0                                                      933
         AG_RMAX = TRAY_COOLOUTR                                          933
         AG_TITLE = 'TUBE'                                                933
         AG_EXNAME = 'SHAPE'                                              933
         AG_PARLIST = 'RMIN_RMAX'                                         933
         CALL AXSHAPE                                                     933
C    *                                                                    933
         AG_RMIN = 0                                                      933
         AG_RMAX = TRAY_COOLINNR                                          934
C    Create BWAT :                                                        934
C    Call To Local Procedure BWAT                                         934
         ASSIGN 5950 TO BLOCK_BWAT                                        934
         GO TO  5820                                                      934
5950     CONTINUE                                                         934
C    *                                                                    934
C    Geant POSITION : BWAT ===> inherited arguments used <===             935
         CALL AGSRESET                                                    935
         AG_TITLE = 'BWAT'                                                935
         AG_EXNAME = 'POSITION'                                           935
         AG_PARLIST = 'NONE'                                              935
         CALL AXPOSITION                                                  935
C    *                                                                    935
5931  CALL AGSPOPB                                                        936
      GO TO BLOCK_BCOV, (5930 ,5940)                                      936
C End of Local Procedure                                                  936
C ----------------------------------------------------------------------  937
C ------------ BWAT                                                       937
C Begin Local Procedure BWAT                                              938
C BWAT IS TPC COOLING WATER                                               938
5820  CONTINUE                                                            938
         AG_TITLE='BWAT'                                                  938
         AG_MODULE=MODULE                                                 938
         AG_MARK='V00'                                                    938
         CALL AGSPUSH                                                     938
         AG_MODULE=MODULE                                                 938
C    *                                                                    938
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       939
         CALL AGSRESET                                                    939
         AG_SEEN = 0                                                      939
         AG_COLO = 3                                                      939
         AG_TITLE = 'BWAT'                                                939
         AG_EXNAME = 'ATTRIBUTE'                                          939
         AG_PARLIST = 'SEEN_COLO'                                         939
         CALL AXATTRIBUTE                                                 939
C    *                                                                    939
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           939
         CALL AGSRESET                                                    940
         AG_A = 1                                                         940
         AG_Z = 1                                                         940
         AG_W = 2                                                         940
         AG_TITLE = 'H2'                                                  940
         AG_EXNAME = 'COMPONENT'                                          940
         AG_PARLIST = 'A_Z_W'                                             940
         CALL AXCOMPONENT                                                 940
C    *                                                                    940
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           940
         CALL AGSRESET                                                    941
         AG_A = 16                                                        941
         AG_Z = 8                                                         941
         AG_W = 1                                                         941
         AG_TITLE = 'O'                                                   941
         AG_EXNAME = 'COMPONENT'                                          941
         AG_PARLIST = 'A_Z_W'                                             941
         CALL AXCOMPONENT                                                 941
C    *                                                                    941
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             941
         CALL AGSRESET                                                    942
         AG_DENS = 1.0                                                    942
         AG_TITLE = 'WATER'                                               942
         AG_EXNAME = 'MIXTURE'                                            942
         AG_PARLIST = 'DENS'                                              942
         CALL AXMIXTURE                                                   942
C    *                                                                    942
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 <===                           942
         CALL AGSRESET                                                    943
         AG_RMIN = 0                                                      943
         AG_RMAX = 0                                                      943
         AG_TITLE = 'TUBE'                                                943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'RMIN_RMAX'                                         943
         CALL AXSHAPE                                                     943
C    *                                                                    943
5821  CALL AGSPOPB                                                        944
      GO TO BLOCK_BWAT, (5820 ,5830,5840,5950)                            944
C End of Local Procedure                                                  944
C ----------------------------------------------------------------------  945
C ------------ BRMD                                                       945
C Begin Local Procedure BRMD                                              946
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   946
5490  CONTINUE                                                            946
         AG_TITLE='BRMD'                                                  946
         AG_MODULE=MODULE                                                 946
         AG_MARK='V00'                                                    946
         CALL AGSPUSH                                                     946
         AG_MODULE=MODULE                                                 946
C    *                                                                    946
C    Geant ATTRIBUTE : BRMD ===> SEEN=0 COLO=6 <===                       947
         CALL AGSRESET                                                    947
         AG_SEEN = 0                                                      947
         AG_COLO = 6                                                      947
         AG_TITLE = 'BRMD'                                                947
         AG_EXNAME = 'ATTRIBUTE'                                          947
         AG_PARLIST = 'SEEN_COLO'                                         947
         CALL AXATTRIBUTE                                                 947
C    *                                                                    947
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           947
         CALL AGSRESET                                                    948
         AG_TITLE = 'RPCGAS'                                              948
         AG_EXNAME = 'MATERIAL'                                           948
         AG_PARLIST = 'NONE'                                              948
         CALL AXMATERIAL                                                  948
C    *                                                                    948
C    Geant SHAPE : BOX ===> DX=MODR_HEIGHT/2 DY=MODR_WIDTH/2, DZ=MODR_LE  948
C    NGTH/2 <===                                                          948
         CALL AGSRESET                                                    950
         AG_DX = MODR_HEIGHT/2                                            950
         AG_DY = MODR_WIDTH/2                                             950
         AG_DZ = MODR_LENGTH/2                                            950
         AG_TITLE = 'BOX'                                                 950
         AG_EXNAME = 'SHAPE'                                              950
         AG_PARLIST = 'DX_DY_DZ'                                          950
         CALL AXSHAPE                                                     950
C    *                                                                    950
C    Create BRHC :                                                        951
C    Call To Local Procedure BRHC                                         951
         ASSIGN 5970 TO BLOCK_BRHC                                        951
         GO TO  5960                                                      951
5970     CONTINUE                                                         951
C    *                                                                    951
C    Create BRCB :                                                        952
C    Call To Local Procedure BRCB                                         952
         ASSIGN 5990 TO BLOCK_BRCB                                        952
         GO TO  5980                                                      952
5990     CONTINUE                                                         952
C    *                                                                    952
C    Create BRMY :                                                        953
C    Call To Local Procedure BRMY                                         953
         ASSIGN 6010 TO BLOCK_BRMY                                        953
         GO TO  6000                                                      953
6010     CONTINUE                                                         953
C    *                                                                    953
C    Create BRGR :                                                        954
C    Call To Local Procedure BRGR                                         954
         ASSIGN 6030 TO BLOCK_BRGR                                        954
         GO TO  6020                                                      954
6030     CONTINUE                                                         954
C    *                                                                    954
C    Create BROG :                                                        955
C    Call To Local Procedure BROG                                         955
         ASSIGN 6050 TO BLOCK_BROG                                        955
         GO TO  6040                                                      955
6050     CONTINUE                                                         955
C    *                                                                    955
         DTHGT =(MODR_IGHGT + MODR_SPRMAX*2)*(MODR_NGAP - 1) +            956
     *   MODR_SPRMAX*2                                                    956
         AG_DZ = MODR_IGLEN/2.                                            957
         AG_KONLY = 'MANY'                                                957
         AG_DX = DTHGT/2.                                                 957
         AG_DY = MODR_IGWID/2.                                            957
C    Create BRDT :                                                        957
C    Call To Local Procedure BRDT                                         957
         ASSIGN 6070 TO BLOCK_BRDT                                        957
         GO TO  6060                                                      957
6070     CONTINUE                                                         957
C    *                                                                    957
C    Create BRWG :                                                        958
C    Call To Local Procedure BRWG                                         958
         ASSIGN 6090 TO BLOCK_BRWG                                        958
         GO TO  6080                                                      958
6090     CONTINUE                                                         958
C    *                                                                    958
         XPOS = MODR_HEIGHT/2.                                            959
         Y = 0.                                                           960
         Z = MODR_CENTER                                                  961
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    962
         CALL AGSRESET                                                    962
         AG_X = XPOS-MODR_HCHGT/2                                         962
         AG_Z = MODR_CENTER                                               962
         AG_TITLE = 'BRHC'                                                962
         AG_EXNAME = 'POSITION'                                           962
         AG_PARLIST = 'X_Z'                                               962
         CALL AXPOSITION                                                  962
C    *                                                                    962
         XPOS = XPOS - MODR_HCHGT                                         962
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            964
         CALL AGSRESET                                                    964
         AG_X = XPOS-MODR_PCBHGT/2                                        964
         AG_Z = 0.                                                        964
         AG_TITLE = 'BRCB'                                                964
         AG_EXNAME = 'POSITION'                                           964
         AG_PARLIST = 'X_Z'                                               964
         CALL AXPOSITION                                                  964
C    *                                                                    964
         XPOS = XPOS - MODR_PCBHGT                                        964
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 Z=MODR_CENTER <===    966
         CALL AGSRESET                                                    966
         AG_X = XPOS-MODR_MYHGT/2                                         966
         AG_Z = MODR_CENTER                                               966
         AG_TITLE = 'BRMY'                                                966
         AG_EXNAME = 'POSITION'                                           966
         AG_PARLIST = 'X_Z'                                               966
         CALL AXPOSITION                                                  966
C    *                                                                    966
         XPOS = XPOS - MODR_MYHGT                                         966
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  968
         CALL AGSRESET                                                    968
         AG_X = XPOS-MODR_GRHGT/2                                         968
         AG_TITLE = 'BRGR'                                                968
         AG_EXNAME = 'POSITION'                                           968
         AG_PARLIST = 'X'                                                 968
         CALL AXPOSITION                                                  968
C    *                                                                    968
         XPOS = XPOS - MODR_GRHGT                                         968
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  970
         CALL AGSRESET                                                    970
         AG_X = XPOS-MODR_OGHGT/2                                         970
         AG_TITLE = 'BROG'                                                970
         AG_EXNAME = 'POSITION'                                           970
         AG_PARLIST = 'X'                                                 970
         CALL AXPOSITION                                                  970
C    *                                                                    970
         XPOS = XPOS - MODR_OGHGT                                         970
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 <===                       972
         CALL AGSRESET                                                    972
         AG_X = XPOS-DTHGT/2                                              972
         AG_TITLE = 'BRDT'                                                972
         AG_EXNAME = 'POSITION'                                           972
         AG_PARLIST = 'X'                                                 972
         CALL AXPOSITION                                                  972
C    *                                                                    972
         XPOS = XPOS - DTHGT                                              972
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  974
         CALL AGSRESET                                                    974
         AG_X = XPOS-MODR_OGHGT/2                                         974
         AG_TITLE = 'BROG'                                                974
         AG_EXNAME = 'POSITION'                                           974
         AG_PARLIST = 'X'                                                 974
         CALL AXPOSITION                                                  974
C    *                                                                    974
         XPOS = XPOS - MODR_OGHGT                                         974
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  976
         CALL AGSRESET                                                    976
         AG_X = XPOS-MODR_GRHGT/2                                         976
         AG_TITLE = 'BRGR'                                                976
         AG_EXNAME = 'POSITION'                                           976
         AG_PARLIST = 'X'                                                 976
         CALL AXPOSITION                                                  976
C    *                                                                    976
         XPOS = XPOS - MODR_GRHGT                                         976
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 <===                  978
         CALL AGSRESET                                                    978
         AG_X = XPOS-MODR_MYHGT/2                                         978
         AG_TITLE = 'BRMY'                                                978
         AG_EXNAME = 'POSITION'                                           978
         AG_PARLIST = 'X'                                                 978
         CALL AXPOSITION                                                  978
C    *                                                                    978
         XPOS = XPOS - MODR_MYHGT                                         978
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            980
         CALL AGSRESET                                                    980
         AG_X = XPOS-MODR_PCBHGT/2                                        980
         AG_Z = 0.                                                        980
         AG_TITLE = 'BRCB'                                                980
         AG_EXNAME = 'POSITION'                                           980
         AG_PARLIST = 'X_Z'                                               980
         CALL AXPOSITION                                                  980
C    *                                                                    980
         XPOS = XPOS - MODR_PCBHGT                                        980
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    982
         CALL AGSRESET                                                    982
         AG_X = XPOS-MODR_HCHGT/2                                         982
         AG_Z = MODR_CENTER                                               982
         AG_TITLE = 'BRHC'                                                982
         AG_EXNAME = 'POSITION'                                           982
         AG_PARLIST = 'X_Z'                                               982
         CALL AXPOSITION                                                  982
C    *                                                                    982
         XPOS = XPOS - MODR_HCHGT                                         982
5491  CALL AGSPOPB                                                        984
      GO TO BLOCK_BRMD, (5490 ,5500)                                      984
C End of Local Procedure                                                  984
C ----------------------------------------------------------------------  985
C ------------ BRHC                                                       985
C Begin Local Procedure BRHC                                              986
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                986
5960  CONTINUE                                                            986
         AG_TITLE='BRHC'                                                  986
         AG_MODULE=MODULE                                                 986
         AG_MARK='V00'                                                    986
         CALL AGSPUSH                                                     986
         AG_MODULE=MODULE                                                 986
C    *                                                                    986
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       987
         CALL AGSRESET                                                    987
         AG_SEEN = 1                                                      987
         AG_COLO = 1                                                      987
         AG_TITLE = 'BRHC'                                                987
         AG_EXNAME = 'ATTRIBUTE'                                          987
         AG_PARLIST = 'SEEN_COLO'                                         987
         CALL AXATTRIBUTE                                                 987
C    *                                                                    987
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        987
         CALL AGSRESET                                                    988
         AG_TITLE = 'HONEYCOMB'                                           988
         AG_EXNAME = 'MATERIAL'                                           988
         AG_PARLIST = 'NONE'                                              988
         CALL AXMATERIAL                                                  988
C    *                                                                    988
C    Geant SHAPE : BOX ===> DX=MODR_HCHGT/2 DY=MODR_HCWID/2, DZ=MODR_HCL  988
C    EN/2 <===                                                            988
         CALL AGSRESET                                                    990
         AG_DX = MODR_HCHGT/2                                             990
         AG_DY = MODR_HCWID/2                                             990
         AG_DZ = MODR_HCLEN/2                                             990
         AG_TITLE = 'BOX'                                                 990
         AG_EXNAME = 'SHAPE'                                              990
         AG_PARLIST = 'DX_DY_DZ'                                          990
         CALL AXSHAPE                                                     990
C    *                                                                    990
5961  CALL AGSPOPB                                                        991
      GO TO BLOCK_BRHC, (5960 ,5970)                                      991
C End of Local Procedure                                                  991
C ----------------------------------------------------------------------  992
C ------------ BRCB                                                       992
C Begin Local Procedure BRCB                                              993
C BRCB IS THE PCB IN THE TOFR MODULE                                      993
5980  CONTINUE                                                            993
         AG_TITLE='BRCB'                                                  993
         AG_MODULE=MODULE                                                 993
         AG_MARK='V00'                                                    993
         CALL AGSPUSH                                                     993
         AG_MODULE=MODULE                                                 993
C    *                                                                    993
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       994
         CALL AGSRESET                                                    994
         AG_SEEN = 1                                                      994
         AG_COLO = 3                                                      994
         AG_TITLE = 'BRCB'                                                994
         AG_EXNAME = 'ATTRIBUTE'                                          994
         AG_PARLIST = 'SEEN_COLO'                                         994
         CALL AXATTRIBUTE                                                 994
C    *                                                                    994
C    Geant MATERIAL : G10 ===> inherited arguments used <===              994
         CALL AGSRESET                                                    995
         AG_TITLE = 'G10'                                                 995
         AG_EXNAME = 'MATERIAL'                                           995
         AG_PARLIST = 'NONE'                                              995
         CALL AXMATERIAL                                                  995
C    *                                                                    995
C    Geant SHAPE : BOX ===> DX=MODR_PCBHGT/2 DY=MODR_PCBWID/2, DZ=MODR_P  995
C    CBLEN/2 <===                                                         995
         CALL AGSRESET                                                    997
         AG_DX = MODR_PCBHGT/2                                            997
         AG_DY = MODR_PCBWID/2                                            997
         AG_DZ = MODR_PCBLEN/2                                            997
         AG_TITLE = 'BOX'                                                 997
         AG_EXNAME = 'SHAPE'                                              997
         AG_PARLIST = 'DX_DY_DZ'                                          997
         CALL AXSHAPE                                                     997
C    *                                                                    997
5981  CALL AGSPOPB                                                        998
      GO TO BLOCK_BRCB, (5980 ,5990)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ BRMY                                                       999
C Begin Local Procedure BRMY                                              1000
C BRMY IS THE MYLAR IN THE TOFR MODULE                                    1000
6000  CONTINUE                                                            1000
         AG_TITLE='BRMY'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant ATTRIBUTE : BRMY ===> SEEN=0 COLO=1 <===                       1001
         CALL AGSRESET                                                    1001
         AG_SEEN = 0                                                      1001
         AG_COLO = 1                                                      1001
         AG_TITLE = 'BRMY'                                                1001
         AG_EXNAME = 'ATTRIBUTE'                                          1001
         AG_PARLIST = 'SEEN_COLO'                                         1001
         CALL AXATTRIBUTE                                                 1001
C    *                                                                    1001
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            1001
         CALL AGSRESET                                                    1002
         AG_TITLE = 'MYLAR'                                               1002
         AG_EXNAME = 'MATERIAL'                                           1002
         AG_PARLIST = 'NONE'                                              1002
         CALL AXMATERIAL                                                  1002
C    *                                                                    1002
C    Geant SHAPE : BOX ===> DX=MODR_MYHGT/2 DY=MODR_MYWID/2, DZ=MODR_MYL  1002
C    EN/2 <===                                                            1002
         CALL AGSRESET                                                    1004
         AG_DX = MODR_MYHGT/2                                             1004
         AG_DY = MODR_MYWID/2                                             1004
         AG_DZ = MODR_MYLEN/2                                             1004
         AG_TITLE = 'BOX'                                                 1004
         AG_EXNAME = 'SHAPE'                                              1004
         AG_PARLIST = 'DX_DY_DZ'                                          1004
         CALL AXSHAPE                                                     1004
C    *                                                                    1004
6001  CALL AGSPOPB                                                        1005
      GO TO BLOCK_BRMY, (6000 ,6010)                                      1005
C End of Local Procedure                                                  1005
C ----------------------------------------------------------------------  1006
C ------------ BRGR                                                       1006
C Begin Local Procedure BRGR                                              1007
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 1007
6020  CONTINUE                                                            1007
         AG_TITLE='BRGR'                                                  1007
         AG_MODULE=MODULE                                                 1007
         AG_MARK='V00'                                                    1007
         CALL AGSPUSH                                                     1007
         AG_MODULE=MODULE                                                 1007
C    *                                                                    1007
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       1008
         CALL AGSRESET                                                    1008
         AG_SEEN = 0                                                      1008
         AG_COLO = 1                                                      1008
         AG_TITLE = 'BRGR'                                                1008
         AG_EXNAME = 'ATTRIBUTE'                                          1008
         AG_PARLIST = 'SEEN_COLO'                                         1008
         CALL AXATTRIBUTE                                                 1008
C    *                                                                    1008
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1008
         CALL AGSRESET                                                    1009
         AG_TITLE = 'CARBON'                                              1009
         AG_EXNAME = 'MATERIAL'                                           1009
         AG_PARLIST = 'NONE'                                              1009
         CALL AXMATERIAL                                                  1009
C    *                                                                    1009
C    Geant SHAPE : BOX ===> DX=MODR_GRHGT/2 DY=MODR_GRWID/2, DZ=MODR_GRL  1009
C    EN/2 <===                                                            1009
         CALL AGSRESET                                                    1011
         AG_DX = MODR_GRHGT/2                                             1011
         AG_DY = MODR_GRWID/2                                             1011
         AG_DZ = MODR_GRLEN/2                                             1011
         AG_TITLE = 'BOX'                                                 1011
         AG_EXNAME = 'SHAPE'                                              1011
         AG_PARLIST = 'DX_DY_DZ'                                          1011
         CALL AXSHAPE                                                     1011
C    *                                                                    1011
6021  CALL AGSPOPB                                                        1012
      GO TO BLOCK_BRGR, (6020 ,6030)                                      1012
C End of Local Procedure                                                  1012
C ----------------------------------------------------------------------  1013
C ------------ BROG                                                       1013
C Begin Local Procedure BROG                                              1014
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              1014
6040  CONTINUE                                                            1014
         AG_TITLE='BROG'                                                  1014
         AG_MODULE=MODULE                                                 1014
         AG_MARK='V00'                                                    1014
         CALL AGSPUSH                                                     1014
         AG_MODULE=MODULE                                                 1014
C    *                                                                    1014
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       1015
         CALL AGSRESET                                                    1015
         AG_SEEN = 1                                                      1015
         AG_COLO = 7                                                      1015
         AG_TITLE = 'BROG'                                                1015
         AG_EXNAME = 'ATTRIBUTE'                                          1015
         AG_PARLIST = 'SEEN_COLO'                                         1015
         CALL AXATTRIBUTE                                                 1015
C    *                                                                    1015
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        1015
         CALL AGSRESET                                                    1016
         AG_A = 28                                                        1016
         AG_Z = 14                                                        1016
         AG_W = 1.                                                        1016
         AG_TITLE = 'SI'                                                  1016
         AG_EXNAME = 'COMPONENT'                                          1016
         AG_PARLIST = 'A_Z_W'                                             1016
         CALL AXCOMPONENT                                                 1016
C    *                                                                    1016
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          1016
         CALL AGSRESET                                                    1017
         AG_A = 16                                                        1017
         AG_Z = 8                                                         1017
         AG_W = 2.                                                        1017
         AG_TITLE = 'O'                                                   1017
         AG_EXNAME = 'COMPONENT'                                          1017
         AG_PARLIST = 'A_Z_W'                                             1017
         CALL AXCOMPONENT                                                 1017
C    *                                                                    1017
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             1017
         CALL AGSRESET                                                    1018
         AG_DENS = 2.2                                                    1018
         AG_TITLE = 'GLASS'                                               1018
         AG_EXNAME = 'MIXTURE'                                            1018
         AG_PARLIST = 'DENS'                                              1018
         CALL AXMIXTURE                                                   1018
C    *                                                                    1018
C    Geant SHAPE : BOX ===> DX=MODR_OGHGT/2 DY=MODR_OGWID/2, DZ=MODR_OGL  1018
C    EN/2 <===                                                            1018
         CALL AGSRESET                                                    1020
         AG_DX = MODR_OGHGT/2                                             1020
         AG_DY = MODR_OGWID/2                                             1020
         AG_DZ = MODR_OGLEN/2                                             1020
         AG_TITLE = 'BOX'                                                 1020
         AG_EXNAME = 'SHAPE'                                              1020
         AG_PARLIST = 'DX_DY_DZ'                                          1020
         CALL AXSHAPE                                                     1020
C    *                                                                    1020
6041  CALL AGSPOPB                                                        1021
      GO TO BLOCK_BROG, (6040 ,6050)                                      1021
C End of Local Procedure                                                  1021
C ----------------------------------------------------------------------  1022
C ------------ BRDT                                                       1022
C Begin Local Procedure BRDT                                              1023
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     1023
6060  CONTINUE                                                            1023
         AG_TITLE='BRDT'                                                  1023
         AG_MODULE=MODULE                                                 1023
         AG_MARK='V00'                                                    1023
         CALL AGSPUSH                                                     1023
         AG_MODULE=MODULE                                                 1023
C    *                                                                    1023
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            1024
         CALL AGSRESET                                                    1024
         AG_TITLE = 'BRDT'                                                1024
         AG_EXNAME = 'ATTRIBUTE'                                          1024
         AG_PARLIST = 'NONE'                                              1024
         CALL AXATTRIBUTE                                                 1024
C    *                                                                    1024
C    Geant SHAPE : BOX ===> inherited arguments used <===                 1024
         CALL AGSRESET                                                    1025
         AG_TITLE = 'BOX'                                                 1025
         AG_EXNAME = 'SHAPE'                                              1025
         AG_PARLIST = 'NONE'                                              1025
         CALL AXSHAPE                                                     1025
C    *                                                                    1025
         AG_KONLY = 'MANY'                                                1025
C    Create BRIG :                                                        1026
C    Call To Local Procedure BRIG                                         1026
         ASSIGN 6110 TO BLOCK_BRIG                                        1026
         GO TO  6100                                                      1026
6110     CONTINUE                                                         1026
C    *                                                                    1026
         AG_KONLY = 'MANY'                                                1027
C    Create BRSG :                                                        1027
C    Call To Local Procedure BRSG                                         1027
         ASSIGN 6130 TO BLOCK_BRSG                                        1027
         GO TO  6120                                                      1027
6130     CONTINUE                                                         1027
C    *                                                                    1027
         XPOS = DTHGT/2.                                                  1028
         Y = 0.                                                           1029
         Z = 0.                                                           1030
C    *                                                                    1031
         IGAP=1                                                           1031
            GO TO 6143                                                    1031
6141        IGAP=IGAP+1                                                   1031
6143        IF(IGAP-(MODR_NGAP-1).GT.0) GO TO 6142                        1031
C       Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                1032
            CALL AGSRESET                                                 1032
            AG_X = XPOS-MODR_SPRMAX                                       1032
            AG_TITLE = 'BRSG'                                             1032
            AG_EXNAME = 'POSITION'                                        1032
            AG_PARLIST = 'X'                                              1032
            CALL AXPOSITION                                               1032
C       *                                                                 1032
            XPOS = XPOS - MODR_SPRMAX*2.                                  1032
C       Geant POSITION : BRIG ===> X=XPOS-MODR_IGHGT/2 <===               1034
            CALL AGSRESET                                                 1034
            AG_X = XPOS-MODR_IGHGT/2                                      1034
            AG_TITLE = 'BRIG'                                             1034
            AG_EXNAME = 'POSITION'                                        1034
            AG_PARLIST = 'X'                                              1034
            CALL AXPOSITION                                               1034
C       *                                                                 1034
            XPOS = XPOS - MODR_IGHGT                                      1034
         GO TO 6141                                                       1036
6142     CONTINUE                                                         1036
C    Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                   1037
         CALL AGSRESET                                                    1037
         AG_X = XPOS-MODR_SPRMAX                                          1037
         AG_TITLE = 'BRSG'                                                1037
         AG_EXNAME = 'POSITION'                                           1037
         AG_PARLIST = 'X'                                                 1037
         CALL AXPOSITION                                                  1037
C    *                                                                    1037
         XPOS = XPOS - MODR_SPRMAX*2.                                     1037
6061  CALL AGSPOPB                                                        1039
      GO TO BLOCK_BRDT, (6060 ,6070)                                      1039
C End of Local Procedure                                                  1039
C ----------------------------------------------------------------------  1040
C ------------ BRIG                                                       1040
C Begin Local Procedure BRIG                                              1041
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              1041
6100  CONTINUE                                                            1041
         AG_TITLE='BRIG'                                                  1041
         AG_MODULE=MODULE                                                 1041
         AG_MARK='V00'                                                    1041
         CALL AGSPUSH                                                     1041
         AG_MODULE=MODULE                                                 1041
C    *                                                                    1041
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       1042
         CALL AGSRESET                                                    1042
         AG_SEEN = 1                                                      1042
         AG_COLO = 7                                                      1042
         AG_TITLE = 'BRIG'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            1042
         CALL AGSRESET                                                    1043
         AG_TITLE = 'GLASS'                                               1043
         AG_EXNAME = 'MATERIAL'                                           1043
         AG_PARLIST = 'NONE'                                              1043
         CALL AXMATERIAL                                                  1043
C    *                                                                    1043
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  1043
C    EN/2 <===                                                            1043
         CALL AGSRESET                                                    1045
         AG_DX = MODR_IGHGT/2                                             1045
         AG_DY = MODR_IGWID/2                                             1045
         AG_DZ = MODR_IGLEN/2                                             1045
         AG_TITLE = 'BOX'                                                 1045
         AG_EXNAME = 'SHAPE'                                              1045
         AG_PARLIST = 'DX_DY_DZ'                                          1045
         CALL AXSHAPE                                                     1045
C    *                                                                    1045
6101  CALL AGSPOPB                                                        1046
      GO TO BLOCK_BRIG, (6100 ,6110)                                      1046
C End of Local Procedure                                                  1046
C ----------------------------------------------------------------------  1047
C ------------ BRSG                                                       1047
C Begin Local Procedure BRSG                                              1048
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      1048
6120  CONTINUE                                                            1048
         AG_TITLE='BRSG'                                                  1048
         AG_MODULE=MODULE                                                 1048
         AG_MARK='V00'                                                    1048
         CALL AGSPUSH                                                     1048
         AG_MODULE=MODULE                                                 1048
C    *                                                                    1048
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       1049
         CALL AGSRESET                                                    1049
         AG_SEEN = 0                                                      1049
         AG_COLO = 5                                                      1049
         AG_TITLE = 'BRSG'                                                1049
         AG_EXNAME = 'ATTRIBUTE'                                          1049
         AG_PARLIST = 'SEEN_COLO'                                         1049
         CALL AXATTRIBUTE                                                 1049
C    *                                                                    1049
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1049
         CALL AGSRESET                                                    1050
         AG_TITLE = 'RPCGAS'                                              1050
         AG_EXNAME = 'MATERIAL'                                           1050
         AG_PARLIST = 'NONE'                                              1050
         CALL AXMATERIAL                                                  1050
C    *                                                                    1050
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1050
         CALL AGSRESET                                                    1051
         AG_ISVOL = 1                                                     1051
         AG_TITLE = 'SENSITIVE'                                           1051
         AG_EXNAME = 'MEDIUM'                                             1051
         AG_PARLIST = 'ISVOL'                                             1051
         CALL AXMEDIUM                                                    1051
C    *                                                                    1051
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  1051
C    EN/2 <===                                                            1051
         CALL AGSRESET                                                    1053
         AG_DX = MODR_IGHGT/2                                             1053
         AG_DY = MODR_IGWID/2                                             1053
         AG_DZ = MODR_IGLEN/2                                             1053
         AG_TITLE = 'BOX'                                                 1053
         AG_EXNAME = 'SHAPE'                                              1053
         AG_PARLIST = 'DX_DY_DZ'                                          1053
         CALL AXSHAPE                                                     1053
C    *                                                                    1053
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  1053
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    1053
         AG_TITLE = 'BRSG'                                                1055
         AG_EXNAME = 'HITS'                                               1055
         AG_TYPE=AG_MAGIC                                                 1055
         CALL AGSDETV('BRSG')                                             1055
C    *                                                                    1055
         AG_PARLIST='X'                                                   1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = AG_MAGIC                                              1055
         AG_BIN=0.01                                                      1055
         AG_OPTION='HS'                                                   1055
         AG_FHMIN = AG_MAGIC                                              1055
         AG_FHMAX = AG_MAGIC                                              1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='Y'                                                   1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = AG_MAGIC                                              1055
         AG_BIN=0.01                                                      1055
         AG_FHMIN=AG_MAGIC                                                1055
         AG_FHMAX=AG_MAGIC                                                1055
         AG_OPTION='*'                                                    1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='Z'                                                   1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = AG_MAGIC                                              1055
         AG_BIN=0.01                                                      1055
         AG_FHMIN=AG_MAGIC                                                1055
         AG_FHMAX=AG_MAGIC                                                1055
         AG_OPTION='*'                                                    1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='PTOT'                                                1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = 18                                                    1055
         AG_BIN=AG_MAGIC                                                  1055
         AG_OPTION='*'                                                    1055
         AG_FHMIN = 0.0                                                   1055
         AG_FHMAX = 100.0                                                 1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='SLENG'                                               1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = AG_MAGIC                                              1055
         AG_BIN=0.1                                                       1055
         AG_OPTION='*'                                                    1055
         AG_FHMIN = 0                                                     1055
         AG_FHMAX = 500                                                   1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='TOF'                                                 1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = 16                                                    1055
         AG_BIN=AG_MAGIC                                                  1055
         AG_OPTION='*'                                                    1055
         AG_FHMIN = 0.0                                                   1055
         AG_FHMAX = 1.0E-7                                                1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='STEP'                                                1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = AG_MAGIC                                              1055
         AG_BIN=0.01                                                      1055
         AG_FHMIN=AG_MAGIC                                                1055
         AG_FHMAX=AG_MAGIC                                                1055
         AG_OPTION='*'                                                    1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         AG_PARLIST='ELOSS'                                               1055
         AG_TITLE = 'DETH'                                                1055
         AG_NBITS = 16                                                    1055
         AG_BIN=AG_MAGIC                                                  1055
         AG_OPTION='*'                                                    1055
         AG_FHMIN = 0.0                                                   1055
         AG_FHMAX = 1.0E-6                                                1055
         CALL AGSDETH (0)                                                 1055
C    *                                                                    1055
         CALL AGSDETU('BRSG')                                             1055
C    *                                                                    1055
6121  CALL AGSPOPB                                                        1056
      GO TO BLOCK_BRSG, (6120 ,6130)                                      1056
C End of Local Procedure                                                  1056
C ----------------------------------------------------------------------  1057
C ------------ BRWG                                                       1057
C Begin Local Procedure BRWG                                              1058
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           1058
6080  CONTINUE                                                            1058
         AG_TITLE='BRWG'                                                  1058
         AG_MODULE=MODULE                                                 1058
         AG_MARK='V00'                                                    1058
         CALL AGSPUSH                                                     1058
         AG_MODULE=MODULE                                                 1058
C    *                                                                    1058
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              1059
         CALL AGSRESET                                                    1059
         AG_SEEN = 0                                                      1059
         AG_TITLE = 'BRWG'                                                1059
         AG_EXNAME = 'ATTRIBUTE'                                          1059
         AG_PARLIST = 'SEEN'                                              1059
         CALL AXATTRIBUTE                                                 1059
C    *                                                                    1059
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           1059
         CALL AGSRESET                                                    1060
         AG_A = 16                                                        1060
         AG_Z = 8                                                         1060
         AG_W = 2                                                         1060
         AG_TITLE = 'O'                                                   1060
         AG_EXNAME = 'COMPONENT'                                          1060
         AG_PARLIST = 'A_Z_W'                                             1060
         CALL AXCOMPONENT                                                 1060
C    *                                                                    1060
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           1060
         CALL AGSRESET                                                    1061
         AG_A = 12                                                        1061
         AG_Z = 6                                                         1061
         AG_W = 5                                                         1061
         AG_TITLE = 'C'                                                   1061
         AG_EXNAME = 'COMPONENT'                                          1061
         AG_PARLIST = 'A_Z_W'                                             1061
         CALL AXCOMPONENT                                                 1061
C    *                                                                    1061
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            1061
         CALL AGSRESET                                                    1062
         AG_A = 1                                                         1062
         AG_Z = 1                                                         1062
         AG_W = 8                                                         1062
         AG_TITLE = 'H'                                                   1062
         AG_EXNAME = 'COMPONENT'                                          1062
         AG_PARLIST = 'A_Z_W'                                             1062
         CALL AXCOMPONENT                                                 1062
C    *                                                                    1062
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           1062
         CALL AGSRESET                                                    1063
         AG_DENS = 1.18                                                   1063
         AG_TITLE = 'LUCITE'                                              1063
         AG_EXNAME = 'MIXTURE'                                            1063
         AG_PARLIST = 'DENS'                                              1063
         CALL AXMIXTURE                                                   1063
C    *                                                                    1063
C    Geant SHAPE : TUBE ===> RMIN=MODR_WGRMIN RMAX=MODR_WGRMAX, DZ=MODR_  1063
C    WGLEN/2 <===                                                         1063
         CALL AGSRESET                                                    1065
         AG_RMIN = MODR_WGRMIN                                            1065
         AG_RMAX = MODR_WGRMAX                                            1065
         AG_DZ = MODR_WGLEN/2                                             1065
         AG_TITLE = 'TUBE'                                                1065
         AG_EXNAME = 'SHAPE'                                              1065
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1065
         CALL AXSHAPE                                                     1065
C    *                                                                    1065
6081  CALL AGSPOPB                                                        1066
      GO TO BLOCK_BRWG, (6080 ,6090)                                      1066
C End of Local Procedure                                                  1066
      WRITE(LOUT,6150)'===>>  [End AgML Module BTOFGEO5]  <<==='          1066
6150  FORMAT(A32,/,/)                                                     1066
C MODULE BTOFGEO5                                                         1068
      END                                                                 1068
