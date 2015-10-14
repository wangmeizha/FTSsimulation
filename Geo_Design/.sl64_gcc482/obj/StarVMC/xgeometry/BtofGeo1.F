C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO1                                                 1
C BTOFGEO1 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO1'/, BANK_TITLE/'*'/                            1
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
C AUTHOR W.J. LLOPE, GEARY EPPLEY, HARLAN HOWE, PABLO YEPES               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'W.J. LLOPE, GEARY EPPLEY, HARLAN HOWE, PABLO  2
     * YEPES'/                                                            2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 23 MARCH 1996                                                   2
      CHARACTER*40 CREATED /'23 MARCH 1996'/                              3
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
      INTEGER BLOCK_BTOH                                                  8
      INTEGER BLOCK_BSEC                                                  8
      INTEGER BLOCK_BTRA                                                  8
      INTEGER BLOCK_BUND                                                  8
      INTEGER BLOCK_BTFT                                                  8
      INTEGER BLOCK_BASE                                                  8
      INTEGER BLOCK_BARM                                                  8
      INTEGER BLOCK_BANG                                                  8
      INTEGER BLOCK_BWAT                                                  8
      INTEGER BLOCK_BCOV                                                  8
      INTEGER BLOCK_BXTR                                                  8
      INTEGER BLOCK_BMTC                                                  8
      INTEGER BLOCK_BTTC                                                  8
      INTEGER BLOCK_BMTM                                                  8
      INTEGER BLOCK_BMTD                                                  8
      INTEGER BLOCK_BASS                                                  8
      INTEGER BLOCK_BXSA                                                  8
      INTEGER BLOCK_BCSB                                                  8
      INTEGER BLOCK_BCCV                                                  8
      INTEGER BLOCK_BCPM                                                  8
      INTEGER BLOCK_BCSK                                                  8
      INTEGER BLOCK_BTSK                                                  8
      INTEGER BLOCK_BZEL                                                  8
      INTEGER BLOCK_BCEL                                                  8
      INTEGER BLOCK_BFEE                                                  8
      INTEGER BLOCK_BCOO                                                  8
      INTEGER BLOCK_BRAI                                                  8
      INTEGER BLOCK_BPIP                                                  8
C -                                                                       9
      REAL BTOG_VERSION                                                   9
      EQUIVALENCE (BANK_BTOG(3),BTOG_VERSION,RE_BTOG_VERSION),            9
     *(NAMES_BTOG(1),NBTOG_VERSION)                                       9
      EQUIVALENCE (COMMEN_BTOG(1),CBTOG_VERSION),(MAP_BTOG(1,1),          9
     *MBTOG_VERSION)                                                      9
      CHARACTER CBTOG_VERSION*40/' '/                                     9
      CHARACTER NBTOG_VERSION*12/'R.VERSION'/                             9
      INTEGER MBTOG_VERSION(2)/1,1/                                       9
      REAL RE_BTOG_VERSION                                                9
C -                                                                       9
      REAL BTOG_RMIN                                                      9
      EQUIVALENCE (BANK_BTOG(4),BTOG_RMIN,RE_BTOG_RMIN),(NAMES_BTOG(2),   9
     *NBTOG_RMIN)                                                         9
      EQUIVALENCE (COMMEN_BTOG(2),CBTOG_RMIN),(MAP_BTOG(1,2),             9
     *MBTOG_RMIN)                                                         9
      CHARACTER CBTOG_RMIN*40/' '/                                        9
      CHARACTER NBTOG_RMIN*12/'R.RMIN'/                                   9
      INTEGER MBTOG_RMIN(2)/1,1/                                          9
      REAL RE_BTOG_RMIN                                                   9
C -                                                                       9
      REAL BTOG_RMAX                                                      9
      EQUIVALENCE (BANK_BTOG(5),BTOG_RMAX,RE_BTOG_RMAX),(NAMES_BTOG(3),   9
     *NBTOG_RMAX)                                                         9
      EQUIVALENCE (COMMEN_BTOG(3),CBTOG_RMAX),(MAP_BTOG(1,3),             9
     *MBTOG_RMAX)                                                         9
      CHARACTER CBTOG_RMAX*40/' '/                                        9
      CHARACTER NBTOG_RMAX*12/'R.RMAX'/                                   9
      INTEGER MBTOG_RMAX(2)/1,1/                                          9
      REAL RE_BTOG_RMAX                                                   9
C -                                                                       9
      REAL BTOG_DZ                                                        9
      EQUIVALENCE (BANK_BTOG(6),BTOG_DZ,RE_BTOG_DZ),(NAMES_BTOG(4),       9
     *NBTOG_DZ)                                                           9
      EQUIVALENCE (COMMEN_BTOG(4),CBTOG_DZ),(MAP_BTOG(1,4),MBTOG_DZ)      9
      CHARACTER CBTOG_DZ*40/' '/                                          9
      CHARACTER NBTOG_DZ*12/'R.DZ'/                                       9
      INTEGER MBTOG_DZ(2)/1,1/                                            9
      REAL RE_BTOG_DZ                                                     9
C -                                                                       9
      REAL BTOG_CHOICE                                                    9
      EQUIVALENCE (BANK_BTOG(7),BTOG_CHOICE,RE_BTOG_CHOICE),              9
     *(NAMES_BTOG(5),NBTOG_CHOICE)                                        9
      EQUIVALENCE (COMMEN_BTOG(5),CBTOG_CHOICE),(MAP_BTOG(1,5),           9
     *MBTOG_CHOICE)                                                       9
      CHARACTER CBTOG_CHOICE*40/' '/                                      9
      CHARACTER NBTOG_CHOICE*12/'R.CHOICE'/                               9
      INTEGER MBTOG_CHOICE(2)/1,1/                                        9
      REAL RE_BTOG_CHOICE                                                 9
C -                                                                       9
      REAL BTOG_POSIT1                                                    9
      EQUIVALENCE (BANK_BTOG(8),BTOG_POSIT1,RE_BTOG_POSIT1),              9
     *(NAMES_BTOG(6),NBTOG_POSIT1)                                        9
      EQUIVALENCE (COMMEN_BTOG(6),CBTOG_POSIT1),(MAP_BTOG(1,6),           9
     *MBTOG_POSIT1)                                                       9
      CHARACTER CBTOG_POSIT1*40/' '/                                      9
      CHARACTER NBTOG_POSIT1*12/'R.POSIT1'/                               9
      INTEGER MBTOG_POSIT1(2)/1,1/                                        9
      REAL RE_BTOG_POSIT1                                                 9
C -                                                                       9
      REAL BTOG_POSIT2                                                    9
      EQUIVALENCE (BANK_BTOG(9),BTOG_POSIT2,RE_BTOG_POSIT2),              9
     *(NAMES_BTOG(7),NBTOG_POSIT2)                                        9
      EQUIVALENCE (COMMEN_BTOG(7),CBTOG_POSIT2),(MAP_BTOG(1,7),           9
     *MBTOG_POSIT2)                                                       9
      CHARACTER CBTOG_POSIT2*40/' '/                                      9
      CHARACTER NBTOG_POSIT2*12/'R.POSIT2'/                               9
      INTEGER MBTOG_POSIT2(2)/1,1/                                        9
      REAL RE_BTOG_POSIT2                                                 9
C -                                                                       9
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                9
      PARAMETER (LEN_BTOG=9)                                              9
      PARAMETER (LLEN_BTOG=7)                                             9
      INTEGER FLAG_BTOG/0/,MAP_BTOG(2,LLEN_BTOG),IDNUM_BTOG(10)           9
      CHARACTER NAMES_BTOG(LLEN_BTOG)*12,COMMEN_BTOG(LLEN_BTOG)*40        9
      DATA IDNUM_BTOG/10*0/                                               9
      REAL BANK_BTOG(LEN_BTOG)                                            9
      CHARACTER*60 BPATH_BTOG/' '/                                        9
      SAVE FLAG_BTOG,MAP_BTOG,IDNUM_BTOG,NAMES_BTOG,COMMEN_BTOG,          9
     *BPATH_BTOG                                                          9
      DATA LINK_BTOG/0/,BANK_BTOG/LEN_BTOG*0/                             9
C *                                                                       9
C -                                                                       12
      REAL TRAY_HEIGHT                                                    12
      EQUIVALENCE (BANK_TRAY(3),TRAY_HEIGHT,RE_TRAY_HEIGHT),              12
     *(NAMES_TRAY(1),NTRAY_HEIGHT)                                        12
      EQUIVALENCE (COMMEN_TRAY(1),CTRAY_HEIGHT),(MAP_TRAY(1,1),           12
     *MTRAY_HEIGHT)                                                       12
      CHARACTER CTRAY_HEIGHT*40/' '/                                      12
      CHARACTER NTRAY_HEIGHT*12/'R.HEIGHT'/                               12
      INTEGER MTRAY_HEIGHT(2)/1,1/                                        12
      REAL RE_TRAY_HEIGHT                                                 12
C -                                                                       12
      REAL TRAY_WIDTH                                                     12
      EQUIVALENCE (BANK_TRAY(4),TRAY_WIDTH,RE_TRAY_WIDTH),                12
     *(NAMES_TRAY(2),NTRAY_WIDTH)                                         12
      EQUIVALENCE (COMMEN_TRAY(2),CTRAY_WIDTH),(MAP_TRAY(1,2),            12
     *MTRAY_WIDTH)                                                        12
      CHARACTER CTRAY_WIDTH*40/' '/                                       12
      CHARACTER NTRAY_WIDTH*12/'R.WIDTH'/                                 12
      INTEGER MTRAY_WIDTH(2)/1,1/                                         12
      REAL RE_TRAY_WIDTH                                                  12
C -                                                                       12
      REAL TRAY_LENGTH                                                    12
      EQUIVALENCE (BANK_TRAY(5),TRAY_LENGTH,RE_TRAY_LENGTH),              12
     *(NAMES_TRAY(3),NTRAY_LENGTH)                                        12
      EQUIVALENCE (COMMEN_TRAY(3),CTRAY_LENGTH),(MAP_TRAY(1,3),           12
     *MTRAY_LENGTH)                                                       12
      CHARACTER CTRAY_LENGTH*40/' '/                                      12
      CHARACTER NTRAY_LENGTH*12/'R.LENGTH'/                               12
      INTEGER MTRAY_LENGTH(2)/1,1/                                        12
      REAL RE_TRAY_LENGTH                                                 12
C -                                                                       12
      REAL TRAY_WALLTHK                                                   12
      EQUIVALENCE (BANK_TRAY(6),TRAY_WALLTHK,RE_TRAY_WALLTHK),            12
     *(NAMES_TRAY(4),NTRAY_WALLTHK)                                       12
      EQUIVALENCE (COMMEN_TRAY(4),CTRAY_WALLTHK),(MAP_TRAY(1,4),          12
     *MTRAY_WALLTHK)                                                      12
      CHARACTER CTRAY_WALLTHK*40/' '/                                     12
      CHARACTER NTRAY_WALLTHK*12/'R.WALLTHK'/                             12
      INTEGER MTRAY_WALLTHK(2)/1,1/                                       12
      REAL RE_TRAY_WALLTHK                                                12
C -                                                                       12
      REAL TRAY_SUPFULLH                                                  12
      EQUIVALENCE (BANK_TRAY(7),TRAY_SUPFULLH,RE_TRAY_SUPFULLH),          12
     *(NAMES_TRAY(5),NTRAY_SUPFULLH)                                      12
      EQUIVALENCE (COMMEN_TRAY(5),CTRAY_SUPFULLH),(MAP_TRAY(1,5),         12
     *MTRAY_SUPFULLH)                                                     12
      CHARACTER CTRAY_SUPFULLH*40/' '/                                    12
      CHARACTER NTRAY_SUPFULLH*12/'R.SUPFULLH'/                           12
      INTEGER MTRAY_SUPFULLH(2)/1,1/                                      12
      REAL RE_TRAY_SUPFULLH                                               12
C -                                                                       12
      REAL TRAY_SUPFULLW                                                  12
      EQUIVALENCE (BANK_TRAY(8),TRAY_SUPFULLW,RE_TRAY_SUPFULLW),          12
     *(NAMES_TRAY(6),NTRAY_SUPFULLW)                                      12
      EQUIVALENCE (COMMEN_TRAY(6),CTRAY_SUPFULLW),(MAP_TRAY(1,6),         12
     *MTRAY_SUPFULLW)                                                     12
      CHARACTER CTRAY_SUPFULLW*40/' '/                                    12
      CHARACTER NTRAY_SUPFULLW*12/'R.SUPFULLW'/                           12
      INTEGER MTRAY_SUPFULLW(2)/1,1/                                      12
      REAL RE_TRAY_SUPFULLW                                               12
C -                                                                       12
      REAL TRAY_SUPLEN                                                    12
      EQUIVALENCE (BANK_TRAY(9),TRAY_SUPLEN,RE_TRAY_SUPLEN),              12
     *(NAMES_TRAY(7),NTRAY_SUPLEN)                                        12
      EQUIVALENCE (COMMEN_TRAY(7),CTRAY_SUPLEN),(MAP_TRAY(1,7),           12
     *MTRAY_SUPLEN)                                                       12
      CHARACTER CTRAY_SUPLEN*40/' '/                                      12
      CHARACTER NTRAY_SUPLEN*12/'R.SUPLEN'/                               12
      INTEGER MTRAY_SUPLEN(2)/1,1/                                        12
      REAL RE_TRAY_SUPLEN                                                 12
C -                                                                       12
      REAL TRAY_SUPBASET                                                  12
      EQUIVALENCE (BANK_TRAY(10),TRAY_SUPBASET,RE_TRAY_SUPBASET),         12
     *(NAMES_TRAY(8),NTRAY_SUPBASET)                                      12
      EQUIVALENCE (COMMEN_TRAY(8),CTRAY_SUPBASET),(MAP_TRAY(1,8),         12
     *MTRAY_SUPBASET)                                                     12
      CHARACTER CTRAY_SUPBASET*40/' '/                                    12
      CHARACTER NTRAY_SUPBASET*12/'R.SUPBASET'/                           12
      INTEGER MTRAY_SUPBASET(2)/1,1/                                      12
      REAL RE_TRAY_SUPBASET                                               12
C -                                                                       12
      REAL TRAY_SUPBASEW                                                  12
      EQUIVALENCE (BANK_TRAY(11),TRAY_SUPBASEW,RE_TRAY_SUPBASEW),         12
     *(NAMES_TRAY(9),NTRAY_SUPBASEW)                                      12
      EQUIVALENCE (COMMEN_TRAY(9),CTRAY_SUPBASEW),(MAP_TRAY(1,9),         12
     *MTRAY_SUPBASEW)                                                     12
      CHARACTER CTRAY_SUPBASEW*40/' '/                                    12
      CHARACTER NTRAY_SUPBASEW*12/'R.SUPBASEW'/                           12
      INTEGER MTRAY_SUPBASEW(2)/1,1/                                      12
      REAL RE_TRAY_SUPBASEW                                               12
C -                                                                       12
      REAL TRAY_SUPARMT                                                   12
      EQUIVALENCE (BANK_TRAY(12),TRAY_SUPARMT,RE_TRAY_SUPARMT),           12
     *(NAMES_TRAY(10),NTRAY_SUPARMT)                                      12
      EQUIVALENCE (COMMEN_TRAY(10),CTRAY_SUPARMT),(MAP_TRAY(1,10),        12
     *MTRAY_SUPARMT)                                                      12
      CHARACTER CTRAY_SUPARMT*40/' '/                                     12
      CHARACTER NTRAY_SUPARMT*12/'R.SUPARMT'/                             12
      INTEGER MTRAY_SUPARMT(2)/1,1/                                       12
      REAL RE_TRAY_SUPARMT                                                12
C -                                                                       12
      REAL TRAY_COOLOUTR                                                  12
      EQUIVALENCE (BANK_TRAY(13),TRAY_COOLOUTR,RE_TRAY_COOLOUTR),         12
     *(NAMES_TRAY(11),NTRAY_COOLOUTR)                                     12
      EQUIVALENCE (COMMEN_TRAY(11),CTRAY_COOLOUTR),(MAP_TRAY(1,11),       12
     *MTRAY_COOLOUTR)                                                     12
      CHARACTER CTRAY_COOLOUTR*40/' '/                                    12
      CHARACTER NTRAY_COOLOUTR*12/'R.COOLOUTR'/                           12
      INTEGER MTRAY_COOLOUTR(2)/1,1/                                      12
      REAL RE_TRAY_COOLOUTR                                               12
C -                                                                       12
      REAL TRAY_COOLINNR                                                  12
      EQUIVALENCE (BANK_TRAY(14),TRAY_COOLINNR,RE_TRAY_COOLINNR),         12
     *(NAMES_TRAY(12),NTRAY_COOLINNR)                                     12
      EQUIVALENCE (COMMEN_TRAY(12),CTRAY_COOLINNR),(MAP_TRAY(1,12),       12
     *MTRAY_COOLINNR)                                                     12
      CHARACTER CTRAY_COOLINNR*40/' '/                                    12
      CHARACTER NTRAY_COOLINNR*12/'R.COOLINNR'/                           12
      INTEGER MTRAY_COOLINNR(2)/1,1/                                      12
      REAL RE_TRAY_COOLINNR                                               12
C -                                                                       12
      REAL TRAY_STRIPT                                                    12
      EQUIVALENCE (BANK_TRAY(15),TRAY_STRIPT,RE_TRAY_STRIPT),             12
     *(NAMES_TRAY(13),NTRAY_STRIPT)                                       12
      EQUIVALENCE (COMMEN_TRAY(13),CTRAY_STRIPT),(MAP_TRAY(1,13),         12
     *MTRAY_STRIPT)                                                       12
      CHARACTER CTRAY_STRIPT*40/' '/                                      12
      CHARACTER NTRAY_STRIPT*12/'R.STRIPT'/                               12
      INTEGER MTRAY_STRIPT(2)/1,1/                                        12
      REAL RE_TRAY_STRIPT                                                 12
C -                                                                       12
      REAL TRAY_FOOTINSE                                                  12
      EQUIVALENCE (BANK_TRAY(16),TRAY_FOOTINSE,RE_TRAY_FOOTINSE),         12
     *(NAMES_TRAY(14),NTRAY_FOOTINSE)                                     12
      EQUIVALENCE (COMMEN_TRAY(14),CTRAY_FOOTINSE),(MAP_TRAY(1,14),       12
     *MTRAY_FOOTINSE)                                                     12
      CHARACTER CTRAY_FOOTINSE*40/' '/                                    12
      CHARACTER NTRAY_FOOTINSE*12/'R.FOOTINSE'/                           12
      INTEGER MTRAY_FOOTINSE(2)/1,1/                                      12
      REAL RE_TRAY_FOOTINSE                                               12
C -                                                                       12
      REAL TRAY_FOOTTHK                                                   12
      EQUIVALENCE (BANK_TRAY(17),TRAY_FOOTTHK,RE_TRAY_FOOTTHK),           12
     *(NAMES_TRAY(15),NTRAY_FOOTTHK)                                      12
      EQUIVALENCE (COMMEN_TRAY(15),CTRAY_FOOTTHK),(MAP_TRAY(1,15),        12
     *MTRAY_FOOTTHK)                                                      12
      CHARACTER CTRAY_FOOTTHK*40/' '/                                     12
      CHARACTER NTRAY_FOOTTHK*12/'R.FOOTTHK'/                             12
      INTEGER MTRAY_FOOTTHK(2)/1,1/                                       12
      REAL RE_TRAY_FOOTTHK                                                12
C -                                                                       12
      REAL TRAY_FOOT1LEN                                                  12
      EQUIVALENCE (BANK_TRAY(18),TRAY_FOOT1LEN,RE_TRAY_FOOT1LEN),         12
     *(NAMES_TRAY(16),NTRAY_FOOT1LEN)                                     12
      EQUIVALENCE (COMMEN_TRAY(16),CTRAY_FOOT1LEN),(MAP_TRAY(1,16),       12
     *MTRAY_FOOT1LEN)                                                     12
      CHARACTER CTRAY_FOOT1LEN*40/' '/                                    12
      CHARACTER NTRAY_FOOT1LEN*12/'R.FOOT1LEN'/                           12
      INTEGER MTRAY_FOOT1LEN(2)/1,1/                                      12
      REAL RE_TRAY_FOOT1LEN                                               12
C -                                                                       12
      REAL TRAY_FOOT2THK                                                  12
      EQUIVALENCE (BANK_TRAY(19),TRAY_FOOT2THK,RE_TRAY_FOOT2THK),         12
     *(NAMES_TRAY(17),NTRAY_FOOT2THK)                                     12
      EQUIVALENCE (COMMEN_TRAY(17),CTRAY_FOOT2THK),(MAP_TRAY(1,17),       12
     *MTRAY_FOOT2THK)                                                     12
      CHARACTER CTRAY_FOOT2THK*40/' '/                                    12
      CHARACTER NTRAY_FOOT2THK*12/'R.FOOT2THK'/                           12
      INTEGER MTRAY_FOOT2THK(2)/1,1/                                      12
      REAL RE_TRAY_FOOT2THK                                               12
C -                                                                       12
      REAL TRAY_FOOT3LEN                                                  12
      EQUIVALENCE (BANK_TRAY(20),TRAY_FOOT3LEN,RE_TRAY_FOOT3LEN),         12
     *(NAMES_TRAY(18),NTRAY_FOOT3LEN)                                     12
      EQUIVALENCE (COMMEN_TRAY(18),CTRAY_FOOT3LEN),(MAP_TRAY(1,18),       12
     *MTRAY_FOOT3LEN)                                                     12
      CHARACTER CTRAY_FOOT3LEN*40/' '/                                    12
      CHARACTER NTRAY_FOOT3LEN*12/'R.FOOT3LEN'/                           12
      INTEGER MTRAY_FOOT3LEN(2)/1,1/                                      12
      REAL RE_TRAY_FOOT3LEN                                               12
C -                                                                       12
      INTEGER LEN_TRAY,LLEN_TRAY,LINK_TRAY                                12
      PARAMETER (LEN_TRAY=20)                                             12
      PARAMETER (LLEN_TRAY=18)                                            12
      INTEGER FLAG_TRAY/0/,MAP_TRAY(2,LLEN_TRAY),IDNUM_TRAY(10)           12
      CHARACTER NAMES_TRAY(LLEN_TRAY)*12,COMMEN_TRAY(LLEN_TRAY)*40        12
      DATA IDNUM_TRAY/10*0/                                               12
      REAL BANK_TRAY(LEN_TRAY)                                            12
      CHARACTER*60 BPATH_TRAY/' '/                                        12
      SAVE FLAG_TRAY,MAP_TRAY,IDNUM_TRAY,NAMES_TRAY,COMMEN_TRAY,          12
     *BPATH_TRAY                                                          12
      DATA LINK_TRAY/0/,BANK_TRAY/LEN_TRAY*0/                             12
C *                                                                       12
C -                                                                       15
      REAL CTBB_SLAB1LEN                                                  15
      EQUIVALENCE (BANK_CTBB(3),CTBB_SLAB1LEN,RE_CTBB_SLAB1LEN),          15
     *(NAMES_CTBB(1),NCTBB_SLAB1LEN)                                      15
      EQUIVALENCE (COMMEN_CTBB(1),CCTBB_SLAB1LEN),(MAP_CTBB(1,1),         15
     *MCTBB_SLAB1LEN)                                                     15
      CHARACTER CCTBB_SLAB1LEN*40/' '/                                    15
      CHARACTER NCTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           15
      INTEGER MCTBB_SLAB1LEN(2)/1,1/                                      15
      REAL RE_CTBB_SLAB1LEN                                               15
C -                                                                       15
      REAL CTBB_SLAB2LEN                                                  15
      EQUIVALENCE (BANK_CTBB(4),CTBB_SLAB2LEN,RE_CTBB_SLAB2LEN),          15
     *(NAMES_CTBB(2),NCTBB_SLAB2LEN)                                      15
      EQUIVALENCE (COMMEN_CTBB(2),CCTBB_SLAB2LEN),(MAP_CTBB(1,2),         15
     *MCTBB_SLAB2LEN)                                                     15
      CHARACTER CCTBB_SLAB2LEN*40/' '/                                    15
      CHARACTER NCTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           15
      INTEGER MCTBB_SLAB2LEN(2)/1,1/                                      15
      REAL RE_CTBB_SLAB2LEN                                               15
C -                                                                       15
      REAL CTBB_SLAB1X                                                    15
      EQUIVALENCE (BANK_CTBB(5),CTBB_SLAB1X,RE_CTBB_SLAB1X),              15
     *(NAMES_CTBB(3),NCTBB_SLAB1X)                                        15
      EQUIVALENCE (COMMEN_CTBB(3),CCTBB_SLAB1X),(MAP_CTBB(1,3),           15
     *MCTBB_SLAB1X)                                                       15
      CHARACTER CCTBB_SLAB1X*40/' '/                                      15
      CHARACTER NCTBB_SLAB1X*12/'R.SLAB1X'/                               15
      INTEGER MCTBB_SLAB1X(2)/1,1/                                        15
      REAL RE_CTBB_SLAB1X                                                 15
C -                                                                       15
      REAL CTBB_SLAB2X                                                    15
      EQUIVALENCE (BANK_CTBB(6),CTBB_SLAB2X,RE_CTBB_SLAB2X),              15
     *(NAMES_CTBB(4),NCTBB_SLAB2X)                                        15
      EQUIVALENCE (COMMEN_CTBB(4),CCTBB_SLAB2X),(MAP_CTBB(1,4),           15
     *MCTBB_SLAB2X)                                                       15
      CHARACTER CCTBB_SLAB2X*40/' '/                                      15
      CHARACTER NCTBB_SLAB2X*12/'R.SLAB2X'/                               15
      INTEGER MCTBB_SLAB2X(2)/1,1/                                        15
      REAL RE_CTBB_SLAB2X                                                 15
C -                                                                       15
      REAL CTBB_SLABTHCK                                                  15
      EQUIVALENCE (BANK_CTBB(7),CTBB_SLABTHCK,RE_CTBB_SLABTHCK),          15
     *(NAMES_CTBB(5),NCTBB_SLABTHCK)                                      15
      EQUIVALENCE (COMMEN_CTBB(5),CCTBB_SLABTHCK),(MAP_CTBB(1,5),         15
     *MCTBB_SLABTHCK)                                                     15
      CHARACTER CCTBB_SLABTHCK*40/' '/                                    15
      CHARACTER NCTBB_SLABTHCK*12/'R.SLABTHCK'/                           15
      INTEGER MCTBB_SLABTHCK(2)/1,1/                                      15
      REAL RE_CTBB_SLABTHCK                                               15
C -                                                                       15
      REAL CTBB_SLABWID                                                   15
      EQUIVALENCE (BANK_CTBB(8),CTBB_SLABWID,RE_CTBB_SLABWID),            15
     *(NAMES_CTBB(6),NCTBB_SLABWID)                                       15
      EQUIVALENCE (COMMEN_CTBB(6),CCTBB_SLABWID),(MAP_CTBB(1,6),          15
     *MCTBB_SLABWID)                                                      15
      CHARACTER CCTBB_SLABWID*40/' '/                                     15
      CHARACTER NCTBB_SLABWID*12/'R.SLABWID'/                             15
      INTEGER MCTBB_SLABWID(2)/1,1/                                       15
      REAL RE_CTBB_SLABWID                                                15
C -                                                                       15
      REAL CTBB_CONVLEN                                                   15
      EQUIVALENCE (BANK_CTBB(9),CTBB_CONVLEN,RE_CTBB_CONVLEN),            15
     *(NAMES_CTBB(7),NCTBB_CONVLEN)                                       15
      EQUIVALENCE (COMMEN_CTBB(7),CCTBB_CONVLEN),(MAP_CTBB(1,7),          15
     *MCTBB_CONVLEN)                                                      15
      CHARACTER CCTBB_CONVLEN*40/' '/                                     15
      CHARACTER NCTBB_CONVLEN*12/'R.CONVLEN'/                             15
      INTEGER MCTBB_CONVLEN(2)/1,1/                                       15
      REAL RE_CTBB_CONVLEN                                                15
C -                                                                       15
      REAL CTBB_CONVWIDM                                                  15
      EQUIVALENCE (BANK_CTBB(10),CTBB_CONVWIDM,RE_CTBB_CONVWIDM),         15
     *(NAMES_CTBB(8),NCTBB_CONVWIDM)                                      15
      EQUIVALENCE (COMMEN_CTBB(8),CCTBB_CONVWIDM),(MAP_CTBB(1,8),         15
     *MCTBB_CONVWIDM)                                                     15
      CHARACTER CCTBB_CONVWIDM*40/' '/                                    15
      CHARACTER NCTBB_CONVWIDM*12/'R.CONVWIDM'/                           15
      INTEGER MCTBB_CONVWIDM(2)/1,1/                                      15
      REAL RE_CTBB_CONVWIDM                                               15
C -                                                                       15
      REAL CTBB_CONVTHCK                                                  15
      EQUIVALENCE (BANK_CTBB(11),CTBB_CONVTHCK,RE_CTBB_CONVTHCK),         15
     *(NAMES_CTBB(9),NCTBB_CONVTHCK)                                      15
      EQUIVALENCE (COMMEN_CTBB(9),CCTBB_CONVTHCK),(MAP_CTBB(1,9),         15
     *MCTBB_CONVTHCK)                                                     15
      CHARACTER CCTBB_CONVTHCK*40/' '/                                    15
      CHARACTER NCTBB_CONVTHCK*12/'R.CONVTHCK'/                           15
      INTEGER MCTBB_CONVTHCK(2)/1,1/                                      15
      REAL RE_CTBB_CONVTHCK                                               15
C -                                                                       15
      REAL CTBB_PMTLEN                                                    15
      EQUIVALENCE (BANK_CTBB(12),CTBB_PMTLEN,RE_CTBB_PMTLEN),             15
     *(NAMES_CTBB(10),NCTBB_PMTLEN)                                       15
      EQUIVALENCE (COMMEN_CTBB(10),CCTBB_PMTLEN),(MAP_CTBB(1,10),         15
     *MCTBB_PMTLEN)                                                       15
      CHARACTER CCTBB_PMTLEN*40/' '/                                      15
      CHARACTER NCTBB_PMTLEN*12/'R.PMTLEN'/                               15
      INTEGER MCTBB_PMTLEN(2)/1,1/                                        15
      REAL RE_CTBB_PMTLEN                                                 15
C -                                                                       15
      REAL CTBB_PMTMAXR                                                   15
      EQUIVALENCE (BANK_CTBB(13),CTBB_PMTMAXR,RE_CTBB_PMTMAXR),           15
     *(NAMES_CTBB(11),NCTBB_PMTMAXR)                                      15
      EQUIVALENCE (COMMEN_CTBB(11),CCTBB_PMTMAXR),(MAP_CTBB(1,11),        15
     *MCTBB_PMTMAXR)                                                      15
      CHARACTER CCTBB_PMTMAXR*40/' '/                                     15
      CHARACTER NCTBB_PMTMAXR*12/'R.PMTMAXR'/                             15
      INTEGER MCTBB_PMTMAXR(2)/1,1/                                       15
      REAL RE_CTBB_PMTMAXR                                                15
C -                                                                       15
      REAL CTBB_PMTMINR                                                   15
      EQUIVALENCE (BANK_CTBB(14),CTBB_PMTMINR,RE_CTBB_PMTMINR),           15
     *(NAMES_CTBB(12),NCTBB_PMTMINR)                                      15
      EQUIVALENCE (COMMEN_CTBB(12),CCTBB_PMTMINR),(MAP_CTBB(1,12),        15
     *MCTBB_PMTMINR)                                                      15
      CHARACTER CCTBB_PMTMINR*40/' '/                                     15
      CHARACTER NCTBB_PMTMINR*12/'R.PMTMINR'/                             15
      INTEGER MCTBB_PMTMINR(2)/1,1/                                       15
      REAL RE_CTBB_PMTMINR                                                15
C -                                                                       15
      REAL CTBB_BASELEN                                                   15
      EQUIVALENCE (BANK_CTBB(15),CTBB_BASELEN,RE_CTBB_BASELEN),           15
     *(NAMES_CTBB(13),NCTBB_BASELEN)                                      15
      EQUIVALENCE (COMMEN_CTBB(13),CCTBB_BASELEN),(MAP_CTBB(1,13),        15
     *MCTBB_BASELEN)                                                      15
      CHARACTER CCTBB_BASELEN*40/' '/                                     15
      CHARACTER NCTBB_BASELEN*12/'R.BASELEN'/                             15
      INTEGER MCTBB_BASELEN(2)/1,1/                                       15
      REAL RE_CTBB_BASELEN                                                15
C -                                                                       15
      REAL CTBB_BASEMAXR                                                  15
      EQUIVALENCE (BANK_CTBB(16),CTBB_BASEMAXR,RE_CTBB_BASEMAXR),         15
     *(NAMES_CTBB(14),NCTBB_BASEMAXR)                                     15
      EQUIVALENCE (COMMEN_CTBB(14),CCTBB_BASEMAXR),(MAP_CTBB(1,14),       15
     *MCTBB_BASEMAXR)                                                     15
      CHARACTER CCTBB_BASEMAXR*40/' '/                                    15
      CHARACTER NCTBB_BASEMAXR*12/'R.BASEMAXR'/                           15
      INTEGER MCTBB_BASEMAXR(2)/1,1/                                      15
      REAL RE_CTBB_BASEMAXR                                               15
C -                                                                       15
      REAL CTBB_BASEMINR                                                  15
      EQUIVALENCE (BANK_CTBB(17),CTBB_BASEMINR,RE_CTBB_BASEMINR),         15
     *(NAMES_CTBB(15),NCTBB_BASEMINR)                                     15
      EQUIVALENCE (COMMEN_CTBB(15),CCTBB_BASEMINR),(MAP_CTBB(1,15),       15
     *MCTBB_BASEMINR)                                                     15
      CHARACTER CCTBB_BASEMINR*40/' '/                                    15
      CHARACTER NCTBB_BASEMINR*12/'R.BASEMINR'/                           15
      INTEGER MCTBB_BASEMINR(2)/1,1/                                      15
      REAL RE_CTBB_BASEMINR                                               15
C -                                                                       15
      REAL CTBB_ELECTHCK                                                  15
      EQUIVALENCE (BANK_CTBB(18),CTBB_ELECTHCK,RE_CTBB_ELECTHCK),         15
     *(NAMES_CTBB(16),NCTBB_ELECTHCK)                                     15
      EQUIVALENCE (COMMEN_CTBB(16),CCTBB_ELECTHCK),(MAP_CTBB(1,16),       15
     *MCTBB_ELECTHCK)                                                     15
      CHARACTER CCTBB_ELECTHCK*40/' '/                                    15
      CHARACTER NCTBB_ELECTHCK*12/'R.ELECTHCK'/                           15
      INTEGER MCTBB_ELECTHCK(2)/1,1/                                      15
      REAL RE_CTBB_ELECTHCK                                               15
C -                                                                       15
      REAL CTBB_WRAP                                                      15
      EQUIVALENCE (BANK_CTBB(19),CTBB_WRAP,RE_CTBB_WRAP),                 15
     *(NAMES_CTBB(17),NCTBB_WRAP)                                         15
      EQUIVALENCE (COMMEN_CTBB(17),CCTBB_WRAP),(MAP_CTBB(1,17),           15
     *MCTBB_WRAP)                                                         15
      CHARACTER CCTBB_WRAP*40/' '/                                        15
      CHARACTER NCTBB_WRAP*12/'R.WRAP'/                                   15
      INTEGER MCTBB_WRAP(2)/1,1/                                          15
      REAL RE_CTBB_WRAP                                                   15
C -                                                                       15
      REAL CTBB_SHIM                                                      15
      EQUIVALENCE (BANK_CTBB(20),CTBB_SHIM,RE_CTBB_SHIM),                 15
     *(NAMES_CTBB(18),NCTBB_SHIM)                                         15
      EQUIVALENCE (COMMEN_CTBB(18),CCTBB_SHIM),(MAP_CTBB(1,18),           15
     *MCTBB_SHIM)                                                         15
      CHARACTER CCTBB_SHIM*40/' '/                                        15
      CHARACTER NCTBB_SHIM*12/'R.SHIM'/                                   15
      INTEGER MCTBB_SHIM(2)/1,1/                                          15
      REAL RE_CTBB_SHIM                                                   15
C -                                                                       15
      INTEGER LEN_CTBB,LLEN_CTBB,LINK_CTBB                                15
      PARAMETER (LEN_CTBB=20)                                             15
      PARAMETER (LLEN_CTBB=18)                                            15
      INTEGER FLAG_CTBB/0/,MAP_CTBB(2,LLEN_CTBB),IDNUM_CTBB(10)           15
      CHARACTER NAMES_CTBB(LLEN_CTBB)*12,COMMEN_CTBB(LLEN_CTBB)*40        15
      DATA IDNUM_CTBB/10*0/                                               15
      REAL BANK_CTBB(LEN_CTBB)                                            15
      CHARACTER*60 BPATH_CTBB/' '/                                        15
      SAVE FLAG_CTBB,MAP_CTBB,IDNUM_CTBB,NAMES_CTBB,COMMEN_CTBB,          15
     *BPATH_CTBB                                                          15
      DATA LINK_CTBB/0/,BANK_CTBB/LEN_CTBB*0/                             15
C *                                                                       15
C -                                                                       18
      REAL TOFF_SLAT1LEN                                                  18
      EQUIVALENCE (BANK_TOFF(3),TOFF_SLAT1LEN,RE_TOFF_SLAT1LEN),          18
     *(NAMES_TOFF(1),NTOFF_SLAT1LEN)                                      18
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_SLAT1LEN),(MAP_TOFF(1,1),         18
     *MTOFF_SLAT1LEN)                                                     18
      CHARACTER CTOFF_SLAT1LEN*40/' '/                                    18
      CHARACTER NTOFF_SLAT1LEN*12/'R.SLAT1LEN'/                           18
      INTEGER MTOFF_SLAT1LEN(2)/1,1/                                      18
      REAL RE_TOFF_SLAT1LEN                                               18
C -                                                                       18
      REAL TOFF_SLAT1Z                                                    18
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLAT1Z,RE_TOFF_SLAT1Z),              18
     *(NAMES_TOFF(2),NTOFF_SLAT1Z)                                        18
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLAT1Z),(MAP_TOFF(1,2),           18
     *MTOFF_SLAT1Z)                                                       18
      CHARACTER CTOFF_SLAT1Z*40/' '/                                      18
      CHARACTER NTOFF_SLAT1Z*12/'R.SLAT1Z'/                               18
      INTEGER MTOFF_SLAT1Z(2)/1,1/                                        18
      REAL RE_TOFF_SLAT1Z                                                 18
C -                                                                       18
      REAL TOFF_SLATDZ                                                    18
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLATDZ,RE_TOFF_SLATDZ),              18
     *(NAMES_TOFF(3),NTOFF_SLATDZ)                                        18
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLATDZ),(MAP_TOFF(1,3),           18
     *MTOFF_SLATDZ)                                                       18
      CHARACTER CTOFF_SLATDZ*40/' '/                                      18
      CHARACTER NTOFF_SLATDZ*12/'R.SLATDZ'/                               18
      INTEGER MTOFF_SLATDZ(2)/1,1/                                        18
      REAL RE_TOFF_SLATDZ                                                 18
C -                                                                       18
      REAL TOFF_SLATTHCK                                                  18
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),          18
     *(NAMES_TOFF(4),NTOFF_SLATTHCK)                                      18
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLATTHCK),(MAP_TOFF(1,4),         18
     *MTOFF_SLATTHCK)                                                     18
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    18
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           18
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      18
      REAL RE_TOFF_SLATTHCK                                               18
C -                                                                       18
      REAL TOFF_SLATWID                                                   18
      EQUIVALENCE (BANK_TOFF(7),TOFF_SLATWID,RE_TOFF_SLATWID),            18
     *(NAMES_TOFF(5),NTOFF_SLATWID)                                       18
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLATWID),(MAP_TOFF(1,5),          18
     *MTOFF_SLATWID)                                                      18
      CHARACTER CTOFF_SLATWID*40/' '/                                     18
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             18
      INTEGER MTOFF_SLATWID(2)/1,1/                                       18
      REAL RE_TOFF_SLATWID                                                18
C -                                                                       18
      REAL TOFF_SLATANG                                                   18
      EQUIVALENCE (BANK_TOFF(8),TOFF_SLATANG,RE_TOFF_SLATANG),            18
     *(NAMES_TOFF(6),NTOFF_SLATANG)                                       18
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLATANG),(MAP_TOFF(1,6),          18
     *MTOFF_SLATANG)                                                      18
      CHARACTER CTOFF_SLATANG*40/' '/                                     18
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             18
      INTEGER MTOFF_SLATANG(2)/1,1/                                       18
      REAL RE_TOFF_SLATANG                                                18
C -                                                                       18
      REAL TOFF_PMTLEN                                                    18
      EQUIVALENCE (BANK_TOFF(9),TOFF_PMTLEN,RE_TOFF_PMTLEN),              18
     *(NAMES_TOFF(7),NTOFF_PMTLEN)                                        18
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_PMTLEN),(MAP_TOFF(1,7),           18
     *MTOFF_PMTLEN)                                                       18
      CHARACTER CTOFF_PMTLEN*40/' '/                                      18
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               18
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        18
      REAL RE_TOFF_PMTLEN                                                 18
C -                                                                       18
      REAL TOFF_PMTMAXR                                                   18
      EQUIVALENCE (BANK_TOFF(10),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           18
     *(NAMES_TOFF(8),NTOFF_PMTMAXR)                                       18
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_PMTMAXR),(MAP_TOFF(1,8),          18
     *MTOFF_PMTMAXR)                                                      18
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     18
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             18
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       18
      REAL RE_TOFF_PMTMAXR                                                18
C -                                                                       18
      REAL TOFF_PMTMINR                                                   18
      EQUIVALENCE (BANK_TOFF(11),TOFF_PMTMINR,RE_TOFF_PMTMINR),           18
     *(NAMES_TOFF(9),NTOFF_PMTMINR)                                       18
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_PMTMINR),(MAP_TOFF(1,9),          18
     *MTOFF_PMTMINR)                                                      18
      CHARACTER CTOFF_PMTMINR*40/' '/                                     18
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             18
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       18
      REAL RE_TOFF_PMTMINR                                                18
C -                                                                       18
      REAL TOFF_BASELEN                                                   18
      EQUIVALENCE (BANK_TOFF(12),TOFF_BASELEN,RE_TOFF_BASELEN),           18
     *(NAMES_TOFF(10),NTOFF_BASELEN)                                      18
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_BASELEN),(MAP_TOFF(1,10),        18
     *MTOFF_BASELEN)                                                      18
      CHARACTER CTOFF_BASELEN*40/' '/                                     18
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             18
      INTEGER MTOFF_BASELEN(2)/1,1/                                       18
      REAL RE_TOFF_BASELEN                                                18
C -                                                                       18
      REAL TOFF_BASEMAXR                                                  18
      EQUIVALENCE (BANK_TOFF(13),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         18
     *(NAMES_TOFF(11),NTOFF_BASEMAXR)                                     18
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_BASEMAXR),(MAP_TOFF(1,11),       18
     *MTOFF_BASEMAXR)                                                     18
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    18
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           18
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      18
      REAL RE_TOFF_BASEMAXR                                               18
C -                                                                       18
      REAL TOFF_BASEMINR                                                  18
      EQUIVALENCE (BANK_TOFF(14),TOFF_BASEMINR,RE_TOFF_BASEMINR),         18
     *(NAMES_TOFF(12),NTOFF_BASEMINR)                                     18
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_BASEMINR),(MAP_TOFF(1,12),       18
     *MTOFF_BASEMINR)                                                     18
      CHARACTER CTOFF_BASEMINR*40/' '/                                    18
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           18
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      18
      REAL RE_TOFF_BASEMINR                                               18
C -                                                                       18
      REAL TOFF_ELECX                                                     18
      EQUIVALENCE (BANK_TOFF(15),TOFF_ELECX,RE_TOFF_ELECX),               18
     *(NAMES_TOFF(13),NTOFF_ELECX)                                        18
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_ELECX),(MAP_TOFF(1,13),          18
     *MTOFF_ELECX)                                                        18
      CHARACTER CTOFF_ELECX*40/' '/                                       18
      CHARACTER NTOFF_ELECX*12/'R.ELECX'/                                 18
      INTEGER MTOFF_ELECX(2)/1,1/                                         18
      REAL RE_TOFF_ELECX                                                  18
C -                                                                       18
      REAL TOFF_ELEC1Z                                                    18
      EQUIVALENCE (BANK_TOFF(16),TOFF_ELEC1Z,RE_TOFF_ELEC1Z),             18
     *(NAMES_TOFF(14),NTOFF_ELEC1Z)                                       18
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_ELEC1Z),(MAP_TOFF(1,14),         18
     *MTOFF_ELEC1Z)                                                       18
      CHARACTER CTOFF_ELEC1Z*40/' '/                                      18
      CHARACTER NTOFF_ELEC1Z*12/'R.ELEC1Z'/                               18
      INTEGER MTOFF_ELEC1Z(2)/1,1/                                        18
      REAL RE_TOFF_ELEC1Z                                                 18
C -                                                                       18
      REAL TOFF_ELECDZ                                                    18
      EQUIVALENCE (BANK_TOFF(17),TOFF_ELECDZ,RE_TOFF_ELECDZ),             18
     *(NAMES_TOFF(15),NTOFF_ELECDZ)                                       18
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_ELECDZ),(MAP_TOFF(1,15),         18
     *MTOFF_ELECDZ)                                                       18
      CHARACTER CTOFF_ELECDZ*40/' '/                                      18
      CHARACTER NTOFF_ELECDZ*12/'R.ELECDZ'/                               18
      INTEGER MTOFF_ELECDZ(2)/1,1/                                        18
      REAL RE_TOFF_ELECDZ                                                 18
C -                                                                       18
      REAL TOFF_ELECTHCK                                                  18
      EQUIVALENCE (BANK_TOFF(18),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         18
     *(NAMES_TOFF(16),NTOFF_ELECTHCK)                                     18
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_ELECTHCK),(MAP_TOFF(1,16),       18
     *MTOFF_ELECTHCK)                                                     18
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    18
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           18
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      18
      REAL RE_TOFF_ELECTHCK                                               18
C -                                                                       18
      REAL TOFF_ELECWID                                                   18
      EQUIVALENCE (BANK_TOFF(19),TOFF_ELECWID,RE_TOFF_ELECWID),           18
     *(NAMES_TOFF(17),NTOFF_ELECWID)                                      18
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_ELECWID),(MAP_TOFF(1,17),        18
     *MTOFF_ELECWID)                                                      18
      CHARACTER CTOFF_ELECWID*40/' '/                                     18
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             18
      INTEGER MTOFF_ELECWID(2)/1,1/                                       18
      REAL RE_TOFF_ELECWID                                                18
C -                                                                       18
      REAL TOFF_ELECLEN                                                   18
      EQUIVALENCE (BANK_TOFF(20),TOFF_ELECLEN,RE_TOFF_ELECLEN),           18
     *(NAMES_TOFF(18),NTOFF_ELECLEN)                                      18
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_ELECLEN),(MAP_TOFF(1,18),        18
     *MTOFF_ELECLEN)                                                      18
      CHARACTER CTOFF_ELECLEN*40/' '/                                     18
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             18
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       18
      REAL RE_TOFF_ELECLEN                                                18
C -                                                                       18
      REAL TOFF_RAILTHCK                                                  18
      EQUIVALENCE (BANK_TOFF(21),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         18
     *(NAMES_TOFF(19),NTOFF_RAILTHCK)                                     18
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_RAILTHCK),(MAP_TOFF(1,19),       18
     *MTOFF_RAILTHCK)                                                     18
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    18
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           18
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      18
      REAL RE_TOFF_RAILTHCK                                               18
C -                                                                       18
      REAL TOFF_RAILWID                                                   18
      EQUIVALENCE (BANK_TOFF(22),TOFF_RAILWID,RE_TOFF_RAILWID),           18
     *(NAMES_TOFF(20),NTOFF_RAILWID)                                      18
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_RAILWID),(MAP_TOFF(1,20),        18
     *MTOFF_RAILWID)                                                      18
      CHARACTER CTOFF_RAILWID*40/' '/                                     18
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             18
      INTEGER MTOFF_RAILWID(2)/1,1/                                       18
      REAL RE_TOFF_RAILWID                                                18
C -                                                                       18
      REAL TOFF_COOLINNR                                                  18
      EQUIVALENCE (BANK_TOFF(23),TOFF_COOLINNR,RE_TOFF_COOLINNR),         18
     *(NAMES_TOFF(21),NTOFF_COOLINNR)                                     18
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_COOLINNR),(MAP_TOFF(1,21),       18
     *MTOFF_COOLINNR)                                                     18
      CHARACTER CTOFF_COOLINNR*40/' '/                                    18
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           18
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      18
      REAL RE_TOFF_COOLINNR                                               18
C -                                                                       18
      REAL TOFF_COOLOUTR                                                  18
      EQUIVALENCE (BANK_TOFF(24),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         18
     *(NAMES_TOFF(22),NTOFF_COOLOUTR)                                     18
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_COOLOUTR),(MAP_TOFF(1,22),       18
     *MTOFF_COOLOUTR)                                                     18
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    18
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           18
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      18
      REAL RE_TOFF_COOLOUTR                                               18
C -                                                                       18
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                18
      PARAMETER (LEN_TOFF=24)                                             18
      PARAMETER (LLEN_TOFF=22)                                            18
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           18
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        18
      DATA IDNUM_TOFF/10*0/                                               18
      REAL BANK_TOFF(LEN_TOFF)                                            18
      CHARACTER*60 BPATH_TOFF/' '/                                        18
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          18
     *BPATH_TOFF                                                          18
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             18
C *                                                                       18
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           20
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        20
     *ZPBASS,ZPFEE                                                        20
      INTEGER I,IS,CHOICE,TOF, YEAR                                       22
C *                                                                       24
      CALL AGSBEGM(MODULE,IPRIN)                                          24
      IF (IPRIN.LT.0) RETURN                                              24
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            24
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               24
      BPATH_BTOG='BTOG*'                                                  24
      IDNUM_BTOG(1)=0                                                     25
C GEOMETRY VERSION                                                        25
      BTOG_VERSION=2                                                      25
      CBTOG_VERSION='GEOMETRY VERSION'                                    25
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                26
      BTOG_RMIN=207.80                                                    26
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               26
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           27
      BTOG_RMAX=219.5                                                     27
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          27
C CTB/TOF TUBE HALF LENGTH                                                28
      BTOG_DZ=246.0                                                       28
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 28
C 1=CTB, 2=TOF, 3=25% TOF+CTB, 4=1 TRAY TOF+CTB                           29
      BTOG_CHOICE=4                                                       29
      CBTOG_CHOICE='1=CTB, 2=TOF, 3=25% TOF+CTB, 4=1 TRAY TOF+CTB'        29
C TOF TRAY POSITION FOR CHOICE 4                                          30
      BTOG_POSIT1=24                                                      30
      CBTOG_POSIT1='TOF TRAY POSITION FOR CHOICE 4'                       30
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    31
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           31
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   31
C *                                                                       31
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    31
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           31
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   31
C *                                                                       32
      CALL AGSBEGM(MODULE,IPRIN)                                          32
      IF (IPRIN.LT.0) RETURN                                              32
C Filling bank TRAY - GENERAL TRAY STATS                                  32
      BANK_TITLE='GENERAL TRAY STATS'                                     32
      BPATH_TRAY='TRAY*'                                                  32
      IDNUM_TRAY(1)=0                                                     33
C TRAY HEIGHT                                                             33
      TRAY_HEIGHT=8.89                                                    33
      CTRAY_HEIGHT='TRAY HEIGHT'                                          33
C FULL TRAY WIDTH                                                         34
      TRAY_WIDTH=21.59                                                    34
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       34
C FULL TRAY LENGTH                                                        35
      TRAY_LENGTH=241.62                                                  35
      CTRAY_LENGTH='FULL TRAY LENGTH'                                     35
C TRAY WALL THICKNESS                                                     36
      TRAY_WALLTHK=0.13                                                   36
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 36
C SUPPORT HEIGHT (RADIAL)                                                 37
      TRAY_SUPFULLH=2.03                                                  37
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            37
C SUPPORT FULL WIDTH WITH ARMS                                            38
      TRAY_SUPFULLW=15.24                                                 38
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       38
C SUPPORT LENGTH                                                          39
      TRAY_SUPLEN=215.9                                                   39
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       39
C SUPPORT BASE WIDTH                                                      40
      TRAY_SUPBASEW=9.22                                                  40
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 40
C SUPPORT BASE THICKNESS                                                  41
      TRAY_SUPBASET=0.32                                                  41
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             41
C SUPPORT ARM THICKNESS                                                   42
      TRAY_SUPARMT=0.64                                                   42
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               42
C COOLING CHANNEL OUTER RADIUS                                            43
      TRAY_COOLOUTR=0.80                                                  43
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       43
C COOLING CHANNEL INNER RADIUS                                            44
      TRAY_COOLINNR=0.48                                                  44
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       44
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               45
      TRAY_STRIPT=0.08                                                    45
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            45
C FOOT INSET FROM TRAY EDGE                                               46
      TRAY_FOOTINSE=1.06                                                  46
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          46
C THICKNESS OF FOOT MATERIAL                                              47
      TRAY_FOOTTHK=0.23                                                   47
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          47
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               48
      TRAY_FOOT1LEN=1.68                                                  48
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          48
C THICKNESS OF SECOND FOOT SECTION                                        49
      TRAY_FOOT2THK=1.16                                                  49
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   49
C LENGTH OF THIRD PART OF FOOT                                            50
      TRAY_FOOT3LEN=2.16                                                  50
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       50
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    51
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           51
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   51
C *                                                                       51
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    51
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           51
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   51
C *                                                                       52
      CALL AGSBEGM(MODULE,IPRIN)                                          52
      IF (IPRIN.LT.0) RETURN                                              52
C Filling bank CTBB - BARREL TRIGGER STATS                                52
      BANK_TITLE='BARREL TRIGGER STATS'                                   52
      BPATH_CTBB='CTBB*'                                                  52
      IDNUM_CTBB(1)=0                                                     53
C FIRST SLAB (B) LENGTH                                                   53
      CTBB_SLAB1LEN=112.5                                                 53
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              53
C SECOND SLAB (A)LENGTH                                                   54
      CTBB_SLAB2LEN=130.0                                                 54
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              54
C FIRST SLAB (B) X POSITION                                               55
      CTBB_SLAB1X=5.84                                                    55
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            55
C SECOND SLAB (A) X POSITION                                              56
      CTBB_SLAB2X=2.67                                                    56
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           56
C SCINTILLATOR SLAB THICKNESSES                                           57
      CTBB_SLABTHCK=1.0                                                   57
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      57
C SCINTILLATOR SLAB WIDTH                                                 58
      CTBB_SLABWID=21.0                                                   58
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             58
C OPTICAL CONVERTER LENGTH                                                59
      CTBB_CONVLEN=8.5                                                    59
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            59
C OPTICAL CONVERTOR MIN WIDTH                                             60
      CTBB_CONVWIDM=4.0                                                   60
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        60
C OPTICAL CONVERTOR THICKNESS                                             61
      CTBB_CONVTHCK=0.92                                                  61
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        61
C PMT LENGTH                                                              62
      CTBB_PMTLEN=5.0                                                     62
      CCTBB_PMTLEN='PMT LENGTH'                                           62
C PMT MAX RADIUS                                                          63
      CTBB_PMTMAXR=2.0                                                    63
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      63
C PMT MIN RADIUS                                                          64
      CTBB_PMTMINR=1.84                                                   64
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      64
C BASE LENGTH                                                             65
      CTBB_BASELEN=4.0                                                    65
      CCTBB_BASELEN='BASE LENGTH'                                         65
C BASE MAX RADIUS                                                         66
      CTBB_BASEMAXR=2.13                                                  66
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    66
C BASE MIN RADIUS                                                         67
      CTBB_BASEMINR=1.0                                                   67
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    67
C READOUT ELECTRONICS THICKNESS                                           68
      CTBB_ELECTHCK=0.25                                                  68
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      68
C THICKNESS OF TYVEK + BLACK PLASTIC                                      69
      CTBB_WRAP=0.13                                                      69
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     69
C THICKNESS OF SHIM TO POSITION SLAT 2                                    70
      CTBB_SHIM=0.26                                                      70
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   70
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    71
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           71
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   71
C *                                                                       71
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    71
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           71
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   71
C *                                                                       72
      CALL AGSBEGM(MODULE,IPRIN)                                          72
      IF (IPRIN.LT.0) RETURN                                              72
C Filling bank TOFF - TIME OF FLIGHT STATS                                72
      BANK_TITLE='TIME OF FLIGHT STATS'                                   72
      BPATH_TOFF='TOFF*'                                                  72
      IDNUM_TOFF(1)=0                                                     73
C SLAT LENGTH                                                             73
      TOFF_SLAT1LEN=22.0                                                  73
      CTOFF_SLAT1LEN='SLAT LENGTH'                                        73
C SLAT 1 Z POSITION                                                       74
      TOFF_SLAT1Z=101.5                                                   74
      CTOFF_SLAT1Z='SLAT 1 Z POSITION'                                    74
C SLAT Z SEPARATION (SAME AS TOFF_ELECDZ)                                 75
      TOFF_SLATDZ=24.                                                     75
      CTOFF_SLATDZ='SLAT Z SEPARATION (SAME AS TOFF_ELECDZ)'              75
C SCINTILLATOR SLAB THICKNESSES                                           76
      TOFF_SLATTHCK=2.0                                                   76
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      76
C SCINTILLATOR SLAB WIDTH                                                 77
      TOFF_SLATWID=4.0                                                    77
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH'                             77
C SLAT ASSY. ANGLE W.R.T. TRAY                                            78
      TOFF_SLATANG=8.                                                     78
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        78
C PMT LENGTH                                                              79
      TOFF_PMTLEN=5.0                                                     79
      CTOFF_PMTLEN='PMT LENGTH'                                           79
C PMT MAX RADIUS                                                          80
      TOFF_PMTMAXR=1.91                                                   80
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      80
C PMT MIN RADIUS                                                          81
      TOFF_PMTMINR=1.8                                                    81
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      81
C BASE LENGTH                                                             82
      TOFF_BASELEN=5.1                                                    82
      CTOFF_BASELEN='BASE LENGTH'                                         82
C BASE MAX RADIUS                                                         83
      TOFF_BASEMAXR=1.91                                                  83
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    83
C BASE MIN RADIUS                                                         84
      TOFF_BASEMINR=1.8                                                   84
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    84
C FEE BOARD X POSITION                                                    85
      TOFF_ELECX=4.1                                                      85
      CTOFF_ELECX='FEE BOARD X POSITION'                                  85
C FEE BOARD 1 Z POSITION                                                  86
      TOFF_ELEC1Z=104.0                                                   86
      CTOFF_ELEC1Z='FEE BOARD 1 Z POSITION'                               86
C FEE BOARD DZ (SAME AS TOFF_SLATDZ)                                      87
      TOFF_ELECDZ=TOFF_SLATDZ                                             87
      CTOFF_ELECDZ='FEE BOARD DZ (SAME AS TOFF_SLATDZ)'                   87
C FEE BOARD THICKNESS (67 MILS)                                           88
      TOFF_ELECTHCK=0.17                                                  88
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      88
C FEE BOARD WIDTH                                                         89
      TOFF_ELECWID=21.0                                                   89
      CTOFF_ELECWID='FEE BOARD WIDTH'                                     89
C FEE BOARD LENGTH                                                        90
      TOFF_ELECLEN=16.0                                                   90
      CTOFF_ELECLEN='FEE BOARD LENGTH'                                    90
C COOLING LOOP RAIL THICKNESS                                             91
      TOFF_RAILTHCK=0.2                                                   91
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        91
C COOLING LOOP RAIL WIDTH                                                 92
      TOFF_RAILWID=1.0                                                    92
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             92
C COOLING LOOP PIPE OUTER RADIUS                                          93
      TOFF_COOLOUTR=0.375                                                 93
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS'                     93
C COOLING LOOP PIPE INNER RADIUS                                          94
      TOFF_COOLINNR=0.350                                                 94
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS'                     94
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    95
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           95
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   95
C *                                                                       95
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    95
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           95
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   95
C use default BTOG bank                                                   96
      BPATH_BTOG='BTOG*'                                                  96
      IDNUM_BTOG(1)=0                                                     96
      CALL AGDGETP('BTOFGEO1','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  96
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      96
     *LINK_BTOG,FLAG_BTOG)                                                96
C use default TRAY bank                                                   97
      BPATH_TRAY='TRAY*'                                                  97
      IDNUM_TRAY(1)=0                                                     97
      CALL AGDGETP('BTOFGEO1','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  97
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      97
     *LINK_TRAY,FLAG_TRAY)                                                97
C use default CTBB bank                                                   98
      BPATH_CTBB='CTBB*'                                                  98
      IDNUM_CTBB(1)=0                                                     98
      CALL AGDGETP('BTOFGEO1','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  98
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      98
     *LINK_CTBB,FLAG_CTBB)                                                98
C use default TOFF bank                                                   99
      BPATH_TOFF='TOFF*'                                                  99
      IDNUM_TOFF(1)=0                                                     99
      CALL AGDGETP('BTOFGEO1','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  99
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      99
     *LINK_TOFF,FLAG_TOFF)                                                99
      CALL AGSBEGM(MODULE,IPRIN)                                          100
C Check IPRIN<0                                                           100
      IF (IPRIN.LT.0) RETURN                                              100
C Create BTOF :                                                           100
C Call To Local Procedure BTOF                                            100
      ASSIGN 5020 TO BLOCK_BTOF                                           100
      GO TO  5010                                                         100
5020  CONTINUE                                                            100
C *                                                                       100
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           101
      CALL AGSRESET                                                       101
      AG_MOTHER = 'CAVE'                                                  101
      AG_TITLE = 'BTOF'                                                   101
      AG_EXNAME = 'POSITION'                                              101
      AG_PARLIST = 'MOTHER'                                               101
      CALL AXPOSITION                                                     101
C *                                                                       101
C ----------------------------------------------------------------------  101
C ------------ BTOF                                                       101
      CALL AGSENDM                                                        103
      RETURN                                                              103
C Begin Local Procedure BTOF                                              103
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   103
5010  CONTINUE                                                            103
         AG_TITLE='BTOF'                                                  103
         AG_MODULE=MODULE                                                 103
         AG_MARK='V00'                                                    103
         CALL AGSPUSH                                                     103
         AG_MODULE=MODULE                                                 103
C    *                                                                    103
C    Geant ATTRIBUTE : BTOF ===> SEEN=0 COLO=1 <===                       104
         CALL AGSRESET                                                    104
         AG_SEEN = 0                                                      104
         AG_COLO = 1                                                      104
         AG_TITLE = 'BTOF'                                                104
         AG_EXNAME = 'ATTRIBUTE'                                          104
         AG_PARLIST = 'SEEN_COLO'                                         104
         CALL AXATTRIBUTE                                                 104
C    *                                                                    104
C    Geant MATERIAL : AIR ===> inherited arguments used <===              104
         CALL AGSRESET                                                    105
         AG_TITLE = 'AIR'                                                 105
         AG_EXNAME = 'MATERIAL'                                           105
         AG_PARLIST = 'NONE'                                              105
         CALL AXMATERIAL                                                  105
C    *                                                                    105
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           105
         CALL AGSRESET                                                    106
         AG_TITLE = 'STANDARD'                                            106
         AG_EXNAME = 'MEDIUM'                                             106
         AG_PARLIST = 'NONE'                                              106
         CALL AXMEDIUM                                                    106
C    *                                                                    106
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN RMAX=BTOG_RMAX, DZ=BTOG_DZ <  106
C    ===                                                                  106
         CALL AGSRESET                                                    108
         AG_RMIN = BTOG_RMIN                                              108
         AG_RMAX = BTOG_RMAX                                              108
         AG_DZ = BTOG_DZ                                                  108
         AG_TITLE = 'TUBE'                                                108
         AG_EXNAME = 'SHAPE'                                              108
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      108
         CALL AXSHAPE                                                     108
C    *                                                                    108
         CHOICE = BTOG_CHOICE                                             108
C    Create BTOH :                                                        110
C    Call To Local Procedure BTOH                                         110
         ASSIGN 5040 TO BLOCK_BTOH                                        110
         GO TO  5030                                                      110
5040     CONTINUE                                                         110
C    *                                                                    110
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2 ALPHAY=180 <===              111
         CALL AGSRESET                                                    111
         AG_Z = +BTOG_DZ/2                                                111
         AG_ALPHAN=AG_ALPHAN+1                                            111
         AG_ALPHAK(AG_ALPHAN)=2                                           111
         AG_ALPHAL(AG_ALPHAN)=180                                         111
         AG_TITLE = 'BTOH'                                                111
         AG_EXNAME = 'POSITION'                                           111
         AG_PARLIST = 'Z_ALPHAY'                                          111
         CALL AXPOSITION                                                  111
C    *                                                                    111
C    Check (CHOICE!=2)                                                    111
         IF ((CHOICE.NE.2)) THEN                                          112
         CHOICE = 1                                                       113
         ENDIF                                                            114
C    Create BTOH :                                                        115
C    Call To Local Procedure BTOH                                         115
         ASSIGN 5050 TO BLOCK_BTOH                                        115
         GO TO  5030                                                      115
5050     CONTINUE                                                         115
C    *                                                                    115
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2 <===                         116
         CALL AGSRESET                                                    116
         AG_Z = -BTOG_DZ/2                                                116
         AG_TITLE = 'BTOH'                                                116
         AG_EXNAME = 'POSITION'                                           116
         AG_PARLIST = 'Z'                                                 116
         CALL AXPOSITION                                                  116
C    *                                                                    116
5011  CALL AGSPOPB                                                        117
      GO TO BLOCK_BTOF, (5010 ,5020)                                      117
C End of Local Procedure                                                  117
C ----------------------------------------------------------------------  118
C ------------ BTOH                                                       118
C Begin Local Procedure BTOH                                              119
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            119
5030  CONTINUE                                                            119
         AG_TITLE='BTOH'                                                  119
         AG_MODULE=MODULE                                                 119
         AG_MARK='V00'                                                    119
         CALL AGSPUSH                                                     119
         AG_MODULE=MODULE                                                 119
C    *                                                                    119
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         120
         CALL AGSRESET                                                    120
         AG_SEEN = 0                                                      120
         AG_COLO = 1                                                      120
         AG_SERIAL = CHOICE                                               120
         AG_TITLE = 'BTOH'                                                120
         AG_EXNAME = 'ATTRIBUTE'                                          120
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  120
         CALL AXATTRIBUTE                                                 120
C    *                                                                    120
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            120
         CALL AGSRESET                                                    121
         AG_DZ = BTOG_DZ/2                                                121
         AG_TITLE = 'TUBE'                                                121
         AG_EXNAME = 'SHAPE'                                              121
         AG_PARLIST = 'DZ'                                                121
         CALL AXSHAPE                                                     121
C    *                                                                    121
C    *                                                                    121
         IS=1                                                             122
            GO TO 5063                                                    122
5061        IS=IS+1                                                       122
5063        IF(IS-(60).GT.0) GO TO 5062                                   122
            TOF=0                                                         123
C       Check (CHOICE==2)                                                 124
            IF ((CHOICE.EQ.2)) THEN                                       124
            TOF=1                                                         125
            ENDIF                                                         126
C       Check (CHOICE==3&51<=IS&IS<=65)                                   127
            IF ((CHOICE.EQ.3.AND.51.LE.IS.AND.IS.LE.65)) THEN             127
            TOF=1                                                         128
            ENDIF                                                         129
C       Check (CHOICE==4&IS==BTOG_POSIT1)                                 130
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1)) THEN                 130
            TOF=1                                                         131
            ENDIF                                                         132
C       Create BSEC :                                                     133
C       Call To Local Procedure BSEC                                      133
            ASSIGN 5080 TO BLOCK_BSEC                                     133
            GO TO  5070                                                   133
5080        CONTINUE                                                      133
C       *                                                                 133
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   134
            CALL AGSRESET                                                 134
            AG_ALPHAN=AG_ALPHAN+1                                         134
            AG_ALPHAK(AG_ALPHAN)=3                                        134
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 134
            AG_TITLE = 'BSEC'                                             134
            AG_EXNAME = 'POSITION'                                        134
            AG_PARLIST = 'ALPHAZ'                                         134
            CALL AXPOSITION                                               134
C       *                                                                 134
         GO TO 5061                                                       135
5062     CONTINUE                                                         135
5031  CALL AGSPOPB                                                        136
      GO TO BLOCK_BTOH, (5030 ,5040,5050)                                 136
C End of Local Procedure                                                  136
C ----------------------------------------------------------------------  137
C ------------ BSEC                                                       137
C Begin Local Procedure BSEC                                              138
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                138
5070  CONTINUE                                                            138
         AG_TITLE='BSEC'                                                  138
         AG_MODULE=MODULE                                                 138
         AG_MARK='V00'                                                    138
         CALL AGSPUSH                                                     138
         AG_MODULE=MODULE                                                 138
C    *                                                                    138
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            139
         CALL AGSRESET                                                    139
         AG_SEEN = 0                                                      139
         AG_COLO = 1                                                      139
         AG_SERIAL = TOF                                                  139
         AG_TITLE = 'BSEC'                                                139
         AG_EXNAME = 'ATTRIBUTE'                                          139
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  139
         CALL AXATTRIBUTE                                                 139
C    *                                                                    139
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      139
         CALL AGSRESET                                                    140
         AG_PHI1 = -3.0                                                   140
         AG_PHI2 = 3.0                                                    140
         AG_TITLE = 'TUBS'                                                140
         AG_EXNAME = 'SHAPE'                                              140
         AG_PARLIST = 'PHI1_PHI2'                                         140
         CALL AXSHAPE                                                     140
C    *                                                                    140
C    Create BTRA :                                                        141
C    Call To Local Procedure BTRA                                         141
         ASSIGN 5100 TO BLOCK_BTRA                                        141
         GO TO  5090                                                      141
5100     CONTINUE                                                         141
C    *                                                                    141
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  142
C    RAY_STRIPT)/2 <===                                                   142
         CALL AGSRESET                                                    142
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2       142
         AG_TITLE = 'BTRA'                                                142
         AG_EXNAME = 'POSITION'                                           142
         AG_PARLIST = 'X'                                                 142
         CALL AXPOSITION                                                  142
C    *                                                                    142
5071  CALL AGSPOPB                                                        143
      GO TO BLOCK_BSEC, (5070 ,5080)                                      143
C End of Local Procedure                                                  143
C ----------------------------------------------------------------------  144
C ------------ BTRA                                                       144
C Begin Local Procedure BTRA                                              145
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             145
5090  CONTINUE                                                            145
         AG_TITLE='BTRA'                                                  145
         AG_MODULE=MODULE                                                 145
         AG_MARK='V00'                                                    145
         CALL AGSPUSH                                                     145
         AG_MODULE=MODULE                                                 145
C    *                                                                    145
C    Geant ATTRIBUTE : BTRA ===> SEEN=0 COLO=2 <===                       146
         CALL AGSRESET                                                    146
         AG_SEEN = 0                                                      146
         AG_COLO = 2                                                      146
         AG_TITLE = 'BTRA'                                                146
         AG_EXNAME = 'ATTRIBUTE'                                          146
         AG_PARLIST = 'SEEN_COLO'                                         146
         CALL AXATTRIBUTE                                                 146
C    *                                                                    146
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  146
C    , DY=TRAY_WIDTH/2 <===                                               146
         CALL AGSRESET                                                    148
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                148
         AG_DY = TRAY_WIDTH/2                                             148
         AG_TITLE = 'BOX'                                                 148
         AG_EXNAME = 'SHAPE'                                              148
         AG_PARLIST = 'DX_DY'                                             148
         CALL AXSHAPE                                                     148
C    *                                                                    148
C    Create BXTR :                                                        149
C    Call To Local Procedure BXTR                                         149
         ASSIGN 5120 TO BLOCK_BXTR                                        149
         GO TO  5110                                                      149
5120     CONTINUE                                                         149
C    *                                                                    149
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  150
C    DZ-TRAY_LENGTH)/2 <===                                               150
         CALL AGSRESET                                                    150
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             150
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   150
         AG_TITLE = 'BXTR'                                                150
         AG_EXNAME = 'POSITION'                                           150
         AG_PARLIST = 'X_Z'                                               150
         CALL AXPOSITION                                                  150
C    *                                                                    150
C    Create BUND :                                                        151
C    Call To Local Procedure BUND                                         151
         ASSIGN 5140 TO BLOCK_BUND                                        151
         GO TO  5130                                                      151
5140     CONTINUE                                                         151
C    *                                                                    151
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  152
C    Z-TRAY_SUPLEN)/2 <===                                                152
         CALL AGSRESET                                                    152
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              152
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   152
         AG_TITLE = 'BUND'                                                152
         AG_EXNAME = 'POSITION'                                           152
         AG_PARLIST = 'X_Z'                                               152
         CALL AXPOSITION                                                  152
C    *                                                                    152
5091  CALL AGSPOPB                                                        153
      GO TO BLOCK_BTRA, (5090 ,5100)                                      153
C End of Local Procedure                                                  153
C ----------------------------------------------------------------------  154
C ------------ BXTR                                                       154
C Begin Local Procedure BXTR                                              155
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB                                155
5110  CONTINUE                                                            155
         AG_TITLE='BXTR'                                                  155
         AG_MODULE=MODULE                                                 155
         AG_MARK='V00'                                                    155
         CALL AGSPUSH                                                     155
         AG_MODULE=MODULE                                                 155
C    *                                                                    155
C    Geant ATTRIBUTE : BXTR ===> SEEN=1 COLO=2 <===                       156
         CALL AGSRESET                                                    156
         AG_SEEN = 1                                                      156
         AG_COLO = 2                                                      156
         AG_TITLE = 'BXTR'                                                156
         AG_EXNAME = 'ATTRIBUTE'                                          156
         AG_PARLIST = 'SEEN_COLO'                                         156
         CALL AXATTRIBUTE                                                 156
C    *                                                                    156
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        156
         CALL AGSRESET                                                    157
         AG_TITLE = 'ALUMINIUM'                                           157
         AG_EXNAME = 'MATERIAL'                                           157
         AG_PARLIST = 'NONE'                                              157
         CALL AXMATERIAL                                                  157
C    *                                                                    157
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        157
         CALL AGSRESET                                                    158
         AG_DX = TRAY_HEIGHT/2                                            158
         AG_DZ = TRAY_LENGTH/2                                            158
         AG_TITLE = 'BOX'                                                 158
         AG_EXNAME = 'SHAPE'                                              158
         AG_PARLIST = 'DX_DZ'                                             158
         CALL AXSHAPE                                                     158
C    *                                                                    158
C    Create BTTC :                                                        159
C    Call To Local Procedure BTTC                                         159
         ASSIGN 5160 TO BLOCK_BTTC                                        159
         GO TO  5150                                                      159
5160     CONTINUE                                                         159
C    *                                                                    159
C    Geant POSITION : BTTC ===> inherited arguments used <===             160
         CALL AGSRESET                                                    160
         AG_TITLE = 'BTTC'                                                160
         AG_EXNAME = 'POSITION'                                           160
         AG_PARLIST = 'NONE'                                              160
         CALL AXPOSITION                                                  160
C    *                                                                    160
C    Create BMTC :                                                        161
C    Call To Local Procedure BMTC                                         161
         ASSIGN 5180 TO BLOCK_BMTC                                        161
         GO TO  5170                                                      161
5180     CONTINUE                                                         161
C    *                                                                    161
C    Geant POSITION : BMTC ===> inherited arguments used <===             162
         CALL AGSRESET                                                    162
         AG_TITLE = 'BMTC'                                                162
         AG_EXNAME = 'POSITION'                                           162
         AG_PARLIST = 'NONE'                                              162
         CALL AXPOSITION                                                  162
C    *                                                                    162
5111  CALL AGSPOPB                                                        163
      GO TO BLOCK_BXTR, (5110 ,5120)                                      163
C End of Local Procedure                                                  163
C ----------------------------------------------------------------------  164
C ------------ BMTC                                                       164
C Begin Local Procedure BMTC                                              165
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH MANY DETAILS FOR CTB           165
5170  CONTINUE                                                            165
         AG_TITLE='BMTC'                                                  165
         AG_MODULE=MODULE                                                 165
         AG_MARK='V00'                                                    165
         CALL AGSPUSH                                                     165
         AG_MODULE=MODULE                                                 165
C    *                                                                    165
C    Geant ATTRIBUTE : BMTC ===> SEEN=1 COLO=5 <===                       166
         CALL AGSRESET                                                    166
         AG_SEEN = 1                                                      166
         AG_COLO = 5                                                      166
         AG_TITLE = 'BMTC'                                                166
         AG_EXNAME = 'ATTRIBUTE'                                          166
         AG_PARLIST = 'SEEN_COLO'                                         166
         CALL AXATTRIBUTE                                                 166
C    *                                                                    166
C    Geant MATERIAL : AIR ===> inherited arguments used <===              166
         CALL AGSRESET                                                    167
         AG_TITLE = 'AIR'                                                 167
         AG_EXNAME = 'MATERIAL'                                           167
         AG_PARLIST = 'NONE'                                              167
         CALL AXMATERIAL                                                  167
C    *                                                                    167
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  167
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   167
         CALL AGSRESET                                                    169
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               169
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                169
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               169
         AG_TITLE = 'BOX'                                                 169
         AG_EXNAME = 'SHAPE'                                              169
         AG_PARLIST = 'DX_DY_DZ'                                          169
         CALL AXSHAPE                                                     169
C    *                                                                    169
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           169
     *   CTBB_WRAP                                                        169
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             171
C    Create BXSA :                                                        172
C    Call To Local Procedure BXSA                                         172
         ASSIGN 5200 TO BLOCK_BXSA                                        172
         GO TO  5190                                                      172
5200     CONTINUE                                                         172
C    *                                                                    172
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  174
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   174
         CALL AGSRESET                                                    174
         AG_X = XPOS                                                      174
         AG_Z = ZPOS                                                      174
         AG_DZ = CTBB_SLAB1LEN/2                                          174
         AG_DX = CTBB_SLABTHCK/2                                          174
         AG_DY = CTBB_SLABWID/2                                           174
         AG_TITLE = 'BXSA'                                                174
         AG_EXNAME = 'POSITION'                                           174
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      174
         CALL AXPOSITION                                                  174
C    *                                                                    174
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   174
C    Create BCCV :                                                        176
C    Call To Local Procedure BCCV                                         176
         ASSIGN 5220 TO BLOCK_BCCV                                        176
         GO TO  5210                                                      176
5220     CONTINUE                                                         176
C    *                                                                    176
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  178
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  178
C    K/2 <===                                                             178
         CALL AGSRESET                                                    178
         AG_X = XPOS                                                      178
         AG_Z = ZPOS                                                      178
         AG_DX2 = CTBB_SLABTHCK/2                                         178
         AG_DY2 = CTBB_SLABWID/2                                          178
         AG_DY1 = CTBB_CONVWIDM/2                                         178
         AG_DZ = CTBB_CONVLEN/2                                           178
         AG_DX1 = CTBB_SLABTHCK/2                                         178
         AG_TITLE = 'BCCV'                                                178
         AG_EXNAME = 'POSITION'                                           178
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            178
         CALL AXPOSITION                                                  178
C    *                                                                    178
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     178
C    Create BCPM :                                                        180
C    Call To Local Procedure BCPM                                         180
         ASSIGN 5240 TO BLOCK_BCPM                                        180
         GO TO  5230                                                      180
5240     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  182
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      182
         CALL AGSRESET                                                    182
         AG_X = XPOS                                                      182
         AG_Z = ZPOS                                                      182
         AG_RMIN = CTBB_PMTMINR                                           182
         AG_RMAX = CTBB_PMTMAXR                                           182
         AG_DZ = CTBB_PMTLEN/2                                            182
         AG_TITLE = 'BCPM'                                                182
         AG_EXNAME = 'POSITION'                                           182
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  182
         CALL AXPOSITION                                                  182
C    *                                                                    182
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     182
C    Create BCSK :                                                        184
C    Call To Local Procedure BCSK                                         184
         ASSIGN 5260 TO BLOCK_BCSK                                        184
         GO TO  5250                                                      184
5260     CONTINUE                                                         184
C    *                                                                    184
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  186
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   186
         CALL AGSRESET                                                    186
         AG_X = XPOS                                                      186
         AG_Z = ZPOS                                                      186
         AG_RMIN = CTBB_BASEMINR                                          186
         AG_RMAX = CTBB_BASEMAXR                                          186
         AG_DZ = CTBB_BASELEN/2                                           186
         AG_TITLE = 'BCSK'                                                186
         AG_EXNAME = 'POSITION'                                           186
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  186
         CALL AXPOSITION                                                  186
C    *                                                                    186
C    Create BZEL :                                                        187
C    Call To Local Procedure BZEL                                         187
         ASSIGN 5280 TO BLOCK_BZEL                                        187
         GO TO  5270                                                      187
5280     CONTINUE                                                         187
C    *                                                                    187
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  189
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 189
         CALL AGSRESET                                                    189
         AG_X = XPOS                                                      189
         AG_Z = ZPOS                                                      189
         AG_DZ = CTBB_BASELEN/2                                           189
         AG_DX = CTBB_ELECTHCK/2                                          189
         AG_DY = CTBB_BASEMINR-0.1                                        189
         AG_TITLE = 'BZEL'                                                189
         AG_EXNAME = 'POSITION'                                           189
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      189
         CALL AXPOSITION                                                  189
C    *                                                                    189
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           189
     *   CTBB_WRAP - CTBB_SHIM                                            189
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             191
C    Create BXSA :                                                        192
C    Call To Local Procedure BXSA                                         192
         ASSIGN 5290 TO BLOCK_BXSA                                        192
         GO TO  5190                                                      192
5290     CONTINUE                                                         192
C    *                                                                    192
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  194
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  194
         CALL AGSRESET                                                    194
         AG_X = XPOS                                                      194
         AG_Z = -ZPOS                                                     194
         AG_DZ = CTBB_SLAB2LEN/2                                          194
         AG_DX = CTBB_SLABTHCK/2                                          194
         AG_DY = CTBB_SLABWID/2                                           194
         AG_TITLE = 'BXSA'                                                194
         AG_EXNAME = 'POSITION'                                           194
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      194
         CALL AXPOSITION                                                  194
C    *                                                                    194
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   194
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  197
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  197
C    CK/2 ALPHAX=180 <===                                                 197
         CALL AGSRESET                                                    197
         AG_X = XPOS                                                      197
         AG_Z = -ZPOS                                                     197
         AG_DX2 = CTBB_SLABTHCK/2                                         197
         AG_DY2 = CTBB_SLABWID/2                                          197
         AG_DY1 = CTBB_CONVWIDM/2                                         197
         AG_DZ = CTBB_CONVLEN/2                                           197
         AG_DX1 = CTBB_SLABTHCK/2                                         197
         AG_ALPHAN=AG_ALPHAN+1                                            197
         AG_ALPHAK(AG_ALPHAN)=1                                           197
         AG_ALPHAL(AG_ALPHAN)=180                                         197
         AG_TITLE = 'BCCV'                                                197
         AG_EXNAME = 'POSITION'                                           197
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     197
         CALL AXPOSITION                                                  197
C    *                                                                    197
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     197
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  200
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     200
         CALL AGSRESET                                                    200
         AG_X = XPOS                                                      200
         AG_Z = -ZPOS                                                     200
         AG_RMIN = CTBB_PMTMINR                                           200
         AG_RMAX = CTBB_PMTMAXR                                           200
         AG_DZ = CTBB_PMTLEN/2                                            200
         AG_TITLE = 'BCPM'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  200
         CALL AXPOSITION                                                  200
C    *                                                                    200
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     200
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  203
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  203
         CALL AGSRESET                                                    203
         AG_X = XPOS                                                      203
         AG_Z = -ZPOS                                                     203
         AG_RMIN = CTBB_BASEMINR                                          203
         AG_RMAX = CTBB_BASEMAXR                                          203
         AG_DZ = CTBB_BASELEN/2                                           203
         AG_TITLE = 'BCSK'                                                203
         AG_EXNAME = 'POSITION'                                           203
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  203
         CALL AXPOSITION                                                  203
C    *                                                                    203
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  203
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                203
         CALL AGSRESET                                                    205
         AG_X = XPOS                                                      205
         AG_Z = -ZPOS                                                     205
         AG_DZ = CTBB_BASELEN/2                                           205
         AG_DX = CTBB_ELECTHCK/2                                          205
         AG_DY = CTBB_BASEMINR-0.1                                        205
         AG_TITLE = 'BZEL'                                                205
         AG_EXNAME = 'POSITION'                                           205
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      205
         CALL AXPOSITION                                                  205
C    *                                                                    205
5171  CALL AGSPOPB                                                        206
      GO TO BLOCK_BMTC, (5170 ,5180)                                      206
C End of Local Procedure                                                  206
C ----------------------------------------------------------------------  207
C ------------ BTTC                                                       207
C Begin Local Procedure BTTC                                              208
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH MANY DETAILS FOR TOF           208
5150  CONTINUE                                                            208
         AG_TITLE='BTTC'                                                  208
         AG_MODULE=MODULE                                                 208
         AG_MARK='V00'                                                    208
         CALL AGSPUSH                                                     208
         AG_MODULE=MODULE                                                 208
C    *                                                                    208
C    Geant ATTRIBUTE : BTTC ===> SEEN=1 COLO=6 <===                       209
         CALL AGSRESET                                                    209
         AG_SEEN = 1                                                      209
         AG_COLO = 6                                                      209
         AG_TITLE = 'BTTC'                                                209
         AG_EXNAME = 'ATTRIBUTE'                                          209
         AG_PARLIST = 'SEEN_COLO'                                         209
         CALL AXATTRIBUTE                                                 209
C    *                                                                    209
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           209
         CALL AGSRESET                                                    210
         AG_A = 12                                                        210
         AG_Z = 6                                                         210
         AG_W = 1                                                         210
         AG_TITLE = 'C'                                                   210
         AG_EXNAME = 'COMPONENT'                                          210
         AG_PARLIST = 'A_Z_W'                                             210
         CALL AXCOMPONENT                                                 210
C    *                                                                    210
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           210
         CALL AGSRESET                                                    211
         AG_A = 1                                                         211
         AG_Z = 1                                                         211
         AG_W = 2                                                         211
         AG_TITLE = 'H2'                                                  211
         AG_EXNAME = 'COMPONENT'                                          211
         AG_PARLIST = 'A_Z_W'                                             211
         CALL AXCOMPONENT                                                 211
C    *                                                                    211
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       211
         CALL AGSRESET                                                    212
         AG_DENS = 0.048                                                  212
         AG_TITLE = 'LASTAFOAM'                                           212
         AG_EXNAME = 'MIXTURE'                                            212
         AG_PARLIST = 'DENS'                                              212
         CALL AXMIXTURE                                                   212
C    *                                                                    212
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  212
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   212
         CALL AGSRESET                                                    214
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               214
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                214
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               214
         AG_TITLE = 'BOX'                                                 214
         AG_EXNAME = 'SHAPE'                                              214
         AG_PARLIST = 'DX_DY_DZ'                                          214
         CALL AXSHAPE                                                     214
C    *                                                                    214
         AG_DZ = TOFF_ELECLEN/2                                           214
         AG_DX = TOFF_ELECTHCK/2                                          215
         AG_DY = TOFF_ELECWID/2                                           215
C    Create BFEE :                                                        215
C    Call To Local Procedure BFEE                                         215
         ASSIGN 5310 TO BLOCK_BFEE                                        215
         GO TO  5300                                                      215
5310     CONTINUE                                                         215
C    *                                                                    215
         ZPFEE = TOFF_ELEC1Z                                              216
C    *                                                                    217
         I=1                                                              217
            GO TO 5323                                                    217
5321        I=I+1                                                         217
5323        IF(I-(9).GT.0) GO TO 5322                                     217
C       Geant POSITION : BFEE ===> X=TOFF_ELECX Z=ZPFEE <===              218
            CALL AGSRESET                                                 218
            AG_X = TOFF_ELECX                                             218
            AG_Z = ZPFEE                                                  218
            AG_TITLE = 'BFEE'                                             218
            AG_EXNAME = 'POSITION'                                        218
            AG_PARLIST = 'X_Z'                                            218
            CALL AXPOSITION                                               218
C       *                                                                 218
            ZPFEE = ZPFEE - TOFF_ELECDZ                                   218
         GO TO 5321                                                       220
5322     CONTINUE                                                         220
C    Create BCOO :                                                        221
C    Call To Local Procedure BCOO                                         221
         ASSIGN 5340 TO BLOCK_BCOO                                        221
         GO TO  5330                                                      221
5340     CONTINUE                                                         221
C    *                                                                    221
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               222
         CALL AGSRESET                                                    222
         AG_X = 0                                                         222
         AG_Y = 0                                                         222
         AG_DZ = 0                                                        222
         AG_DX = 0                                                        222
         AG_DY = 0                                                        222
         AG_TITLE = 'BCOO'                                                222
         AG_EXNAME = 'POSITION'                                           222
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      222
         CALL AXPOSITION                                                  222
C    *                                                                    222
C    Create BMTM :                                                        223
C    Call To Local Procedure BMTM                                         223
         ASSIGN 5360 TO BLOCK_BMTM                                        223
         GO TO  5350                                                      223
5360     CONTINUE                                                         223
C    *                                                                    223
C    Geant POSITION : BMTM ===> inherited arguments used <===             224
         CALL AGSRESET                                                    224
         AG_TITLE = 'BMTM'                                                224
         AG_EXNAME = 'POSITION'                                           224
         AG_PARLIST = 'NONE'                                              224
         CALL AXPOSITION                                                  224
C    *                                                                    224
5151  CALL AGSPOPB                                                        225
      GO TO BLOCK_BTTC, (5150 ,5160)                                      225
C End of Local Procedure                                                  225
C ----------------------------------------------------------------------  226
C ------------ BMTM                                                       226
C Begin Local Procedure BMTM                                              227
C BMTM IS THE MAIN TRAY CAVITY DIVISIONS MOTHER VOLUME FOR TOF            227
5350  CONTINUE                                                            227
         AG_TITLE='BMTM'                                                  227
         AG_MODULE=MODULE                                                 227
         AG_MARK='V00'                                                    227
         CALL AGSPUSH                                                     227
         AG_MODULE=MODULE                                                 227
C    *                                                                    227
C    Geant ATTRIBUTE : BMTM ===> SEEN=0 COLO=1 <===                       228
         CALL AGSRESET                                                    228
         AG_SEEN = 0                                                      228
         AG_COLO = 1                                                      228
         AG_TITLE = 'BMTM'                                                228
         AG_EXNAME = 'ATTRIBUTE'                                          228
         AG_PARLIST = 'SEEN_COLO'                                         228
         CALL AXATTRIBUTE                                                 228
C    *                                                                    228
C    Geant MATERIAL : AIR ===> inherited arguments used <===              228
         CALL AGSRESET                                                    229
         AG_TITLE = 'AIR'                                                 229
         AG_EXNAME = 'MATERIAL'                                           229
         AG_PARLIST = 'NONE'                                              229
         CALL AXMATERIAL                                                  229
C    *                                                                    229
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  229
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   229
         CALL AGSRESET                                                    231
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               231
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                231
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               231
         AG_TITLE = 'BOX'                                                 231
         AG_EXNAME = 'SHAPE'                                              231
         AG_PARLIST = 'DX_DY_DZ'                                          231
         CALL AXSHAPE                                                     231
C    *                                                                    231
C    Create BMTD :                                                        232
C    Call To Local Procedure BMTD                                         232
         ASSIGN 5380 TO BLOCK_BMTD                                        232
         GO TO  5370                                                      232
5380     CONTINUE                                                         232
C    *                                                                    232
5351  CALL AGSPOPB                                                        233
      GO TO BLOCK_BMTM, (5350 ,5360)                                      233
C End of Local Procedure                                                  233
C ----------------------------------------------------------------------  234
C ------------ BMTD                                                       234
C Begin Local Procedure BMTD                                              235
C BMTD IS A PHI COLUMN OF TOF SCINTILLATORS                               235
5370  CONTINUE                                                            235
         AG_TITLE='BMTD'                                                  235
         AG_MODULE=MODULE                                                 235
         AG_MARK='V00'                                                    235
         CALL AGSPUSH                                                     235
         AG_MODULE=MODULE                                                 235
C    *                                                                    235
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       236
         CALL AGSRESET                                                    236
         AG_SEEN = 1                                                      236
         AG_COLO = 1                                                      236
         AG_TITLE = 'BMTD'                                                236
         AG_EXNAME = 'ATTRIBUTE'                                          236
         AG_PARLIST = 'SEEN_COLO'                                         236
         CALL AXATTRIBUTE                                                 236
C    *                                                                    236
C    Geant SHAPE : DIVISION ===> NDIV=5 IAXIS=2 <===                      236
         CALL AGSRESET                                                    237
         AG_NDIV = 5                                                      237
         AG_IAXIS = 2                                                     237
         AG_TITLE = 'DIVISION'                                            237
         AG_EXNAME = 'SHAPE'                                              237
         AG_PARLIST = 'NDIV_IAXIS'                                        237
         CALL AXSHAPE                                                     237
C    *                                                                    237
C    Create BASS :                                                        238
C    Call To Local Procedure BASS                                         238
         ASSIGN 5400 TO BLOCK_BASS                                        238
         GO TO  5390                                                      238
5400     CONTINUE                                                         238
C    *                                                                    238
         ZPBASS = TOFF_SLAT1Z                                             239
C    *                                                                    240
         I=1                                                              240
            GO TO 5413                                                    240
5411        I=I+1                                                         240
5413        IF(I-(9).GT.0) GO TO 5412                                     240
C       Geant POSITION : BASS ===> X=-0.8 Z=ZPBASS ALPHAY=TOFF_SLATANG K  241
C       ONLY='MANY' <===                                                  241
            CALL AGSRESET                                                 241
            AG_X = -0.8                                                   241
            AG_Z = ZPBASS                                                 241
            AG_ALPHAN=AG_ALPHAN+1                                         241
            AG_ALPHAK(AG_ALPHAN)=2                                        241
            AG_ALPHAL(AG_ALPHAN)=TOFF_SLATANG                             241
            AG_KONLY = 'MANY'                                             241
            AG_TITLE = 'BASS'                                             241
            AG_EXNAME = 'POSITION'                                        241
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               241
            CALL AXPOSITION                                               241
C       *                                                                 241
            ZPBASS = ZPBASS - TOFF_SLATDZ                                 241
         GO TO 5411                                                       243
5412     CONTINUE                                                         243
5371  CALL AGSPOPB                                                        244
      GO TO BLOCK_BMTD, (5370 ,5380)                                      244
C End of Local Procedure                                                  244
C ----------------------------------------------------------------------  245
C ------------ BASS                                                       245
C Begin Local Procedure BASS                                              246
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      246
5390  CONTINUE                                                            246
         AG_TITLE='BASS'                                                  246
         AG_MODULE=MODULE                                                 246
         AG_MARK='V00'                                                    246
         CALL AGSPUSH                                                     246
         AG_MODULE=MODULE                                                 246
C    *                                                                    246
C    Geant ATTRIBUTE : BASS ===> SEEN=1 COLO=6 <===                       247
         CALL AGSRESET                                                    247
         AG_SEEN = 1                                                      247
         AG_COLO = 6                                                      247
         AG_TITLE = 'BASS'                                                247
         AG_EXNAME = 'ATTRIBUTE'                                          247
         AG_PARLIST = 'SEEN_COLO'                                         247
         CALL AXATTRIBUTE                                                 247
C    *                                                                    247
         TOTLEN = TOFF_SLAT1LEN + TOFF_PMTLEN + TOFF_BASELEN              247
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  250
C    K)/5., DZ=TOTLEN/2. <===                                             250
         CALL AGSRESET                                                    250
         AG_DX = TOFF_PMTMAXR                                             250
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           250
         AG_DZ = TOTLEN/2.                                                250
         AG_TITLE = 'BOX'                                                 250
         AG_EXNAME = 'SHAPE'                                              250
         AG_PARLIST = 'DX_DY_DZ'                                          250
         CALL AXSHAPE                                                     250
C    *                                                                    250
         ZPOS = - (TOTLEN - TOFF_SLAT1LEN)/2                              250
C    Create BCSB :                                                        252
C    Call To Local Procedure BCSB                                         252
         ASSIGN 5430 TO BLOCK_BCSB                                        252
         GO TO  5420                                                      252
5430     CONTINUE                                                         252
C    *                                                                    252
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               253
         CALL AGSRESET                                                    253
         AG_Z = ZPOS                                                      253
         AG_TITLE = 'BCSB'                                                253
         AG_EXNAME = 'POSITION'                                           253
         AG_PARLIST = 'Z'                                                 253
         CALL AXPOSITION                                                  253
C    *                                                                    253
         ZPOS = ZPOS + (TOFF_SLAT1LEN + TOFF_PMTLEN)/2                    253
C    Create BCPM :                                                        255
C    Call To Local Procedure BCPM                                         255
         ASSIGN 5440 TO BLOCK_BCPM                                        255
         GO TO  5230                                                      255
5440     CONTINUE                                                         255
C    *                                                                    255
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  256
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         256
         CALL AGSRESET                                                    256
         AG_X = 0                                                         256
         AG_Z = ZPOS                                                      256
         AG_RMIN = TOFF_PMTMINR                                           256
         AG_RMAX = TOFF_PMTMAXR                                           256
         AG_DZ = TOFF_PMTLEN/2                                            256
         AG_TITLE = 'BCPM'                                                256
         AG_EXNAME = 'POSITION'                                           256
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  256
         CALL AXPOSITION                                                  256
C    *                                                                    256
         ZPOS = ZPOS + (TOFF_PMTLEN + TOFF_BASELEN)/2                     256
C    Create BTSK :                                                        258
C    Call To Local Procedure BTSK                                         258
         ASSIGN 5460 TO BLOCK_BTSK                                        258
         GO TO  5450                                                      258
5460     CONTINUE                                                         258
C    *                                                                    258
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  259
C    MTMAXR DZ=TOFF_BASELEN/2 <===                                        259
         CALL AGSRESET                                                    259
         AG_X = 0                                                         259
         AG_Z = ZPOS                                                      259
         AG_RMIN = TOFF_PMTMINR                                           259
         AG_RMAX = TOFF_PMTMAXR                                           259
         AG_DZ = TOFF_BASELEN/2                                           259
         AG_TITLE = 'BTSK'                                                259
         AG_EXNAME = 'POSITION'                                           259
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  259
         CALL AXPOSITION                                                  259
C    *                                                                    259
         AG_RMAX = TOFF_PMTMINR                                           259
         AG_RMIN = 0                                                      260
         AG_DZ = TOFF_ELECTHCK/2                                          260
C    Create BCEL :                                                        260
C    Call To Local Procedure BCEL                                         260
         ASSIGN 5480 TO BLOCK_BCEL                                        260
         GO TO  5470                                                      260
5480     CONTINUE                                                         260
C    *                                                                    260
         ZPOS = ZPOS + 0.4 - TOFF_BASELEN/2                               261
C    *                                                                    262
         I=1                                                              262
            GO TO 5493                                                    262
5491        I=I+1                                                         262
5493        IF(I-(4).GT.0) GO TO 5492                                     262
C       Geant POSITION : BCEL ===> Z=ZPOS+I <===                          263
            CALL AGSRESET                                                 263
            AG_Z = ZPOS+I                                                 263
            AG_TITLE = 'BCEL'                                             263
            AG_EXNAME = 'POSITION'                                        263
            AG_PARLIST = 'Z'                                              263
            CALL AXPOSITION                                               263
C       *                                                                 263
         GO TO 5491                                                       264
5492     CONTINUE                                                         264
5391  CALL AGSPOPB                                                        265
      GO TO BLOCK_BASS, (5390 ,5400)                                      265
C End of Local Procedure                                                  265
C ----------------------------------------------------------------------  266
C ------------ BXSA                                                       266
C Begin Local Procedure BXSA                                              267
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    267
5190  CONTINUE                                                            267
         AG_TITLE='BXSA'                                                  267
         AG_MODULE=MODULE                                                 267
         AG_MARK='V00'                                                    267
         CALL AGSPUSH                                                     267
         AG_MODULE=MODULE                                                 267
C    *                                                                    267
C    Geant ATTRIBUTE : BXSA ===> SEEN=1 COLO=3 <===                       268
         CALL AGSRESET                                                    268
         AG_SEEN = 1                                                      268
         AG_COLO = 3                                                      268
         AG_TITLE = 'BXSA'                                                268
         AG_EXNAME = 'ATTRIBUTE'                                          268
         AG_PARLIST = 'SEEN_COLO'                                         268
         CALL AXATTRIBUTE                                                 268
C    *                                                                    268
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       268
         CALL AGSRESET                                                    269
         AG_TITLE = 'POLYSTYREN'                                          269
         AG_EXNAME = 'MATERIAL'                                           269
         AG_PARLIST = 'NONE'                                              269
         CALL AXMATERIAL                                                  269
C    *                                                                    269
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           269
         CALL AGSRESET                                                    270
         AG_ISVOL = 1                                                     270
         AG_TITLE = 'SENSITIVE'                                           270
         AG_EXNAME = 'MEDIUM'                                             270
         AG_PARLIST = 'ISVOL'                                             270
         CALL AXMEDIUM                                                    270
C    *                                                                    270
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           270
         CALL AGSRESET                                                    271
         AG_DX = 0                                                        271
         AG_DY = 0                                                        271
         AG_DZ = 0                                                        271
         AG_TITLE = 'BOX'                                                 271
         AG_EXNAME = 'SHAPE'                                              271
         AG_PARLIST = 'DX_DY_DZ'                                          271
         CALL AXSHAPE                                                     271
C    *                                                                    271
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   271
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  271
C    SS:16:(0.0,0.01)                                                     271
         AG_TITLE = 'BXSA'                                                273
         AG_EXNAME = 'HITS'                                               273
         AG_TYPE=AG_MAGIC                                                 273
         CALL AGSDETV('BXSA')                                             273
C    *                                                                    273
         AG_PARLIST='X'                                                   273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = AG_MAGIC                                              273
         AG_BIN=0.01                                                      273
         AG_OPTION='S'                                                    273
         AG_FHMIN = AG_MAGIC                                              273
         AG_FHMAX = AG_MAGIC                                              273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='Y'                                                   273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = AG_MAGIC                                              273
         AG_BIN=0.01                                                      273
         AG_FHMIN=AG_MAGIC                                                273
         AG_FHMAX=AG_MAGIC                                                273
         AG_OPTION='*'                                                    273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='Z'                                                   273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = AG_MAGIC                                              273
         AG_BIN=0.01                                                      273
         AG_FHMIN=AG_MAGIC                                                273
         AG_FHMAX=AG_MAGIC                                                273
         AG_OPTION='*'                                                    273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='PTOT'                                                273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = 18                                                    273
         AG_BIN=AG_MAGIC                                                  273
         AG_OPTION='*'                                                    273
         AG_FHMIN = 0.0                                                   273
         AG_FHMAX = 100.0                                                 273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='CX'                                                  273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = 10                                                    273
         AG_BIN=AG_MAGIC                                                  273
         AG_FHMIN=AG_MAGIC                                                273
         AG_FHMAX=AG_MAGIC                                                273
         AG_OPTION='*'                                                    273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='CY'                                                  273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = 10                                                    273
         AG_BIN=AG_MAGIC                                                  273
         AG_FHMIN=AG_MAGIC                                                273
         AG_FHMAX=AG_MAGIC                                                273
         AG_OPTION='*'                                                    273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='CZ'                                                  273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = 10                                                    273
         AG_BIN=AG_MAGIC                                                  273
         AG_OPTION='*'                                                    273
         AG_FHMIN = AG_MAGIC                                              273
         AG_FHMAX = AG_MAGIC                                              273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='SLENG'                                               273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = AG_MAGIC                                              273
         AG_BIN=0.1                                                       273
         AG_OPTION='*'                                                    273
         AG_FHMIN = 0                                                     273
         AG_FHMAX = 500                                                   273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='TOF'                                                 273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = 16                                                    273
         AG_BIN=AG_MAGIC                                                  273
         AG_OPTION='*'                                                    273
         AG_FHMIN = 0.0                                                   273
         AG_FHMAX = 1.0E-6                                                273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='STEP'                                                273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = AG_MAGIC                                              273
         AG_BIN=0.01                                                      273
         AG_FHMIN=AG_MAGIC                                                273
         AG_FHMAX=AG_MAGIC                                                273
         AG_OPTION='*'                                                    273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         AG_PARLIST='ELOSS'                                               273
         AG_TITLE = 'DETH'                                                273
         AG_NBITS = 16                                                    273
         AG_BIN=AG_MAGIC                                                  273
         AG_OPTION='*'                                                    273
         AG_FHMIN = 0.0                                                   273
         AG_FHMAX = 0.01                                                  273
         CALL AGSDETH (0)                                                 273
C    *                                                                    273
         CALL AGSDETU('BXSA')                                             273
C    *                                                                    273
5191  CALL AGSPOPB                                                        274
      GO TO BLOCK_BXSA, (5190 ,5200,5290)                                 274
C End of Local Procedure                                                  274
C ----------------------------------------------------------------------  275
C ------------ BCSB                                                       275
C Begin Local Procedure BCSB                                              276
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    276
5420  CONTINUE                                                            276
         AG_TITLE='BCSB'                                                  276
         AG_MODULE=MODULE                                                 276
         AG_MARK='V00'                                                    276
         CALL AGSPUSH                                                     276
         AG_MODULE=MODULE                                                 276
C    *                                                                    276
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=4 <===                       277
         CALL AGSRESET                                                    277
         AG_SEEN = 1                                                      277
         AG_COLO = 4                                                      277
         AG_TITLE = 'BCSB'                                                277
         AG_EXNAME = 'ATTRIBUTE'                                          277
         AG_PARLIST = 'SEEN_COLO'                                         277
         CALL AXATTRIBUTE                                                 277
C    *                                                                    277
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       277
         CALL AGSRESET                                                    278
         AG_TITLE = 'POLYSTYREN'                                          278
         AG_EXNAME = 'MATERIAL'                                           278
         AG_PARLIST = 'NONE'                                              278
         CALL AXMATERIAL                                                  278
C    *                                                                    278
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           278
         CALL AGSRESET                                                    279
         AG_ISVOL = 1                                                     279
         AG_TITLE = 'SENSITIVE'                                           279
         AG_EXNAME = 'MEDIUM'                                             279
         AG_PARLIST = 'ISVOL'                                             279
         CALL AXMEDIUM                                                    279
C    *                                                                    279
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  279
C    F_SLAT1LEN/2 <===                                                    279
         CALL AGSRESET                                                    281
         AG_DX = TOFF_SLATTHCK/2                                          281
         AG_DY = TOFF_SLATWID/2                                           281
         AG_DZ = TOFF_SLAT1LEN/2                                          281
         AG_TITLE = 'BOX'                                                 281
         AG_EXNAME = 'SHAPE'                                              281
         AG_PARLIST = 'DX_DY_DZ'                                          281
         CALL AXSHAPE                                                     281
C    *                                                                    281
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   281
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  281
C    SS:16:(0.0,0.01)                                                     281
         AG_TITLE = 'BCSB'                                                283
         AG_EXNAME = 'HITS'                                               283
         AG_TYPE=AG_MAGIC                                                 283
         CALL AGSDETV('BCSB')                                             283
C    *                                                                    283
         AG_PARLIST='X'                                                   283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = AG_MAGIC                                              283
         AG_BIN=0.01                                                      283
         AG_OPTION='S'                                                    283
         AG_FHMIN = AG_MAGIC                                              283
         AG_FHMAX = AG_MAGIC                                              283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='Y'                                                   283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = AG_MAGIC                                              283
         AG_BIN=0.01                                                      283
         AG_FHMIN=AG_MAGIC                                                283
         AG_FHMAX=AG_MAGIC                                                283
         AG_OPTION='*'                                                    283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='Z'                                                   283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = AG_MAGIC                                              283
         AG_BIN=0.01                                                      283
         AG_FHMIN=AG_MAGIC                                                283
         AG_FHMAX=AG_MAGIC                                                283
         AG_OPTION='*'                                                    283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='PTOT'                                                283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = 18                                                    283
         AG_BIN=AG_MAGIC                                                  283
         AG_OPTION='*'                                                    283
         AG_FHMIN = 0.0                                                   283
         AG_FHMAX = 100.0                                                 283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='CX'                                                  283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = 10                                                    283
         AG_BIN=AG_MAGIC                                                  283
         AG_FHMIN=AG_MAGIC                                                283
         AG_FHMAX=AG_MAGIC                                                283
         AG_OPTION='*'                                                    283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='CY'                                                  283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = 10                                                    283
         AG_BIN=AG_MAGIC                                                  283
         AG_FHMIN=AG_MAGIC                                                283
         AG_FHMAX=AG_MAGIC                                                283
         AG_OPTION='*'                                                    283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='CZ'                                                  283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = 10                                                    283
         AG_BIN=AG_MAGIC                                                  283
         AG_OPTION='*'                                                    283
         AG_FHMIN = AG_MAGIC                                              283
         AG_FHMAX = AG_MAGIC                                              283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='SLENG'                                               283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = AG_MAGIC                                              283
         AG_BIN=0.1                                                       283
         AG_OPTION='*'                                                    283
         AG_FHMIN = 0                                                     283
         AG_FHMAX = 500                                                   283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='TOF'                                                 283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = 16                                                    283
         AG_BIN=AG_MAGIC                                                  283
         AG_OPTION='*'                                                    283
         AG_FHMIN = 0.0                                                   283
         AG_FHMAX = 1.0E-6                                                283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='STEP'                                                283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = AG_MAGIC                                              283
         AG_BIN=0.01                                                      283
         AG_FHMIN=AG_MAGIC                                                283
         AG_FHMAX=AG_MAGIC                                                283
         AG_OPTION='*'                                                    283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         AG_PARLIST='ELOSS'                                               283
         AG_TITLE = 'DETH'                                                283
         AG_NBITS = 16                                                    283
         AG_BIN=AG_MAGIC                                                  283
         AG_OPTION='*'                                                    283
         AG_FHMIN = 0.0                                                   283
         AG_FHMAX = 0.01                                                  283
         CALL AGSDETH (0)                                                 283
C    *                                                                    283
         CALL AGSDETU('BCSB')                                             283
C    *                                                                    283
5421  CALL AGSPOPB                                                        284
      GO TO BLOCK_BCSB, (5420 ,5430)                                      284
C End of Local Procedure                                                  284
C ----------------------------------------------------------------------  285
C ------------ BCCV                                                       285
C Begin Local Procedure BCCV                                              286
C BCCV IS A CTB OPTICAL CONVERTER                                         286
5210  CONTINUE                                                            286
         AG_TITLE='BCCV'                                                  286
         AG_MODULE=MODULE                                                 286
         AG_MARK='V00'                                                    286
         CALL AGSPUSH                                                     286
         AG_MODULE=MODULE                                                 286
C    *                                                                    286
C    Geant ATTRIBUTE : BCCV ===> SEEN=1 COLO=3 <===                       287
         CALL AGSRESET                                                    287
         AG_SEEN = 1                                                      287
         AG_COLO = 3                                                      287
         AG_TITLE = 'BCCV'                                                287
         AG_EXNAME = 'ATTRIBUTE'                                          287
         AG_PARLIST = 'SEEN_COLO'                                         287
         CALL AXATTRIBUTE                                                 287
C    *                                                                    287
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       287
         CALL AGSRESET                                                    288
         AG_TITLE = 'POLYSTYREN'                                          288
         AG_EXNAME = 'MATERIAL'                                           288
         AG_PARLIST = 'NONE'                                              288
         CALL AXMATERIAL                                                  288
C    *                                                                    288
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            288
         CALL AGSRESET                                                    289
         AG_DX1 = 0                                                       289
         AG_DX2 = 0                                                       289
         AG_DY1 = 0                                                       289
         AG_DY2 = 0                                                       289
         AG_DZ = 0                                                        289
         AG_TITLE = 'TRD2'                                                289
         AG_EXNAME = 'SHAPE'                                              289
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                289
         CALL AXSHAPE                                                     289
C    *                                                                    289
5211  CALL AGSPOPB                                                        290
      GO TO BLOCK_BCCV, (5210 ,5220)                                      290
C End of Local Procedure                                                  290
C ----------------------------------------------------------------------  291
C ------------ BCPM                                                       291
C Begin Local Procedure BCPM                                              292
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   292
5230  CONTINUE                                                            292
         AG_TITLE='BCPM'                                                  292
         AG_MODULE=MODULE                                                 292
         AG_MARK='V00'                                                    292
         CALL AGSPUSH                                                     292
         AG_MODULE=MODULE                                                 292
C    *                                                                    292
C    Geant ATTRIBUTE : BCPM ===> SEEN=1 COLO=1 <===                       293
         CALL AGSRESET                                                    293
         AG_SEEN = 1                                                      293
         AG_COLO = 1                                                      293
         AG_TITLE = 'BCPM'                                                293
         AG_EXNAME = 'ATTRIBUTE'                                          293
         AG_PARLIST = 'SEEN_COLO'                                         293
         CALL AXATTRIBUTE                                                 293
C    *                                                                    293
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       293
         CALL AGSRESET                                                    294
         AG_TITLE = 'POLYSTYREN'                                          294
         AG_EXNAME = 'MATERIAL'                                           294
         AG_PARLIST = 'NONE'                                              294
         CALL AXMATERIAL                                                  294
C    *                                                                    294
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      294
         CALL AGSRESET                                                    295
         AG_RMIN = 0                                                      295
         AG_RMAX = 0                                                      295
         AG_DZ = 0                                                        295
         AG_TITLE = 'TUBE'                                                295
         AG_EXNAME = 'SHAPE'                                              295
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      295
         CALL AXSHAPE                                                     295
C    *                                                                    295
5231  CALL AGSPOPB                                                        296
      GO TO BLOCK_BCPM, (5230 ,5240,5440)                                 296
C End of Local Procedure                                                  296
C ----------------------------------------------------------------------  297
C ------------ BCSK                                                       297
C Begin Local Procedure BCSK                                              298
C BCSK IS A CTB LINEAR BASE TUBE                                          298
5250  CONTINUE                                                            298
         AG_TITLE='BCSK'                                                  298
         AG_MODULE=MODULE                                                 298
         AG_MARK='V00'                                                    298
         CALL AGSPUSH                                                     298
         AG_MODULE=MODULE                                                 298
C    *                                                                    298
C    Geant ATTRIBUTE : BCSK ===> SEEN=1 COLO=2 <===                       299
         CALL AGSRESET                                                    299
         AG_SEEN = 1                                                      299
         AG_COLO = 2                                                      299
         AG_TITLE = 'BCSK'                                                299
         AG_EXNAME = 'ATTRIBUTE'                                          299
         AG_PARLIST = 'SEEN_COLO'                                         299
         CALL AXATTRIBUTE                                                 299
C    *                                                                    299
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       299
         CALL AGSRESET                                                    300
         AG_TITLE = 'POLYSTYREN'                                          300
         AG_EXNAME = 'MATERIAL'                                           300
         AG_PARLIST = 'NONE'                                              300
         CALL AXMATERIAL                                                  300
C    *                                                                    300
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      300
         CALL AGSRESET                                                    301
         AG_RMIN = 0                                                      301
         AG_RMAX = 0                                                      301
         AG_DZ = 0                                                        301
         AG_TITLE = 'TUBE'                                                301
         AG_EXNAME = 'SHAPE'                                              301
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      301
         CALL AXSHAPE                                                     301
C    *                                                                    301
5251  CALL AGSPOPB                                                        302
      GO TO BLOCK_BCSK, (5250 ,5260)                                      302
C End of Local Procedure                                                  302
C ----------------------------------------------------------------------  303
C ------------ BTSK                                                       303
C Begin Local Procedure BTSK                                              304
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                304
5450  CONTINUE                                                            304
         AG_TITLE='BTSK'                                                  304
         AG_MODULE=MODULE                                                 304
         AG_MARK='V00'                                                    304
         CALL AGSPUSH                                                     304
         AG_MODULE=MODULE                                                 304
C    *                                                                    304
C    Geant ATTRIBUTE : BTSK ===> SEEN=1 COLO=2 <===                       305
         CALL AGSRESET                                                    305
         AG_SEEN = 1                                                      305
         AG_COLO = 2                                                      305
         AG_TITLE = 'BTSK'                                                305
         AG_EXNAME = 'ATTRIBUTE'                                          305
         AG_PARLIST = 'SEEN_COLO'                                         305
         CALL AXATTRIBUTE                                                 305
C    *                                                                    305
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        305
         CALL AGSRESET                                                    306
         AG_TITLE = 'ALUMINIUM'                                           306
         AG_EXNAME = 'MATERIAL'                                           306
         AG_PARLIST = 'NONE'                                              306
         CALL AXMATERIAL                                                  306
C    *                                                                    306
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      306
         CALL AGSRESET                                                    307
         AG_RMIN = 0                                                      307
         AG_RMAX = 0                                                      307
         AG_DZ = 0                                                        307
         AG_TITLE = 'TUBE'                                                307
         AG_EXNAME = 'SHAPE'                                              307
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      307
         CALL AXSHAPE                                                     307
C    *                                                                    307
5451  CALL AGSPOPB                                                        308
      GO TO BLOCK_BTSK, (5450 ,5460)                                      308
C End of Local Procedure                                                  308
C ----------------------------------------------------------------------  309
C ------------ BZEL                                                       309
C Begin Local Procedure BZEL                                              310
C BZEL IS A CTB PM ELECTRONICS                                            310
5270  CONTINUE                                                            310
         AG_TITLE='BZEL'                                                  310
         AG_MODULE=MODULE                                                 310
         AG_MARK='V00'                                                    310
         CALL AGSPUSH                                                     310
         AG_MODULE=MODULE                                                 310
C    *                                                                    310
C    Geant ATTRIBUTE : BZEL ===> SEEN=1 COLO=6 <===                       311
         CALL AGSRESET                                                    311
         AG_SEEN = 1                                                      311
         AG_COLO = 6                                                      311
         AG_TITLE = 'BZEL'                                                311
         AG_EXNAME = 'ATTRIBUTE'                                          311
         AG_PARLIST = 'SEEN_COLO'                                         311
         CALL AXATTRIBUTE                                                 311
C    *                                                                    311
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          311
         CALL AGSRESET                                                    312
         AG_TITLE = 'SILICON'                                             312
         AG_EXNAME = 'MATERIAL'                                           312
         AG_PARLIST = 'NONE'                                              312
         CALL AXMATERIAL                                                  312
C    *                                                                    312
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           312
         CALL AGSRESET                                                    313
         AG_DX = 0                                                        313
         AG_DY = 0                                                        313
         AG_DZ = 0                                                        313
         AG_TITLE = 'BOX'                                                 313
         AG_EXNAME = 'SHAPE'                                              313
         AG_PARLIST = 'DX_DY_DZ'                                          313
         CALL AXSHAPE                                                     313
C    *                                                                    313
5271  CALL AGSPOPB                                                        314
      GO TO BLOCK_BZEL, (5270 ,5280)                                      314
C End of Local Procedure                                                  314
C ----------------------------------------------------------------------  315
C ------------ BCEL                                                       315
C Begin Local Procedure BCEL                                              316
C BCEL IS A G10 BOARD IN THE CW BASE FOR TOF                              316
5470  CONTINUE                                                            316
         AG_TITLE='BCEL'                                                  316
         AG_MODULE=MODULE                                                 316
         AG_MARK='V00'                                                    316
         CALL AGSPUSH                                                     316
         AG_MODULE=MODULE                                                 316
C    *                                                                    316
C    Geant ATTRIBUTE : BCEL ===> SEEN=1 COLO=3 <===                       317
         CALL AGSRESET                                                    317
         AG_SEEN = 1                                                      317
         AG_COLO = 3                                                      317
         AG_TITLE = 'BCEL'                                                317
         AG_EXNAME = 'ATTRIBUTE'                                          317
         AG_PARLIST = 'SEEN_COLO'                                         317
         CALL AXATTRIBUTE                                                 317
C    *                                                                    317
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          317
         CALL AGSRESET                                                    318
         AG_A = 28.08                                                     318
         AG_Z = 14                                                        318
         AG_W = 0.6*1*28./60.                                             318
         AG_TITLE = 'SI'                                                  318
         AG_EXNAME = 'COMPONENT'                                          318
         AG_PARLIST = 'A_Z_W'                                             318
         CALL AXCOMPONENT                                                 318
C    *                                                                    318
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   318
C    <===                                                                 318
         CALL AGSRESET                                                    319
         AG_A = 16                                                        319
         AG_Z = 8                                                         319
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            319
         AG_TITLE = 'O'                                                   319
         AG_EXNAME = 'COMPONENT'                                          319
         AG_PARLIST = 'A_Z_W'                                             319
         CALL AXCOMPONENT                                                 319
C    *                                                                    319
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              319
         CALL AGSRESET                                                    320
         AG_A = 12                                                        320
         AG_Z = 6                                                         320
         AG_W = 0.4*8*12./174.                                            320
         AG_TITLE = 'C'                                                   320
         AG_EXNAME = 'COMPONENT'                                          320
         AG_PARLIST = 'A_Z_W'                                             320
         CALL AXCOMPONENT                                                 320
C    *                                                                    320
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               320
         CALL AGSRESET                                                    321
         AG_A = 1                                                         321
         AG_Z = 1                                                         321
         AG_W = 0.4*14*1./174.                                            321
         AG_TITLE = 'H'                                                   321
         AG_EXNAME = 'COMPONENT'                                          321
         AG_PARLIST = 'A_Z_W'                                             321
         CALL AXCOMPONENT                                                 321
C    *                                                                    321
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               321
         CALL AGSRESET                                                    322
         AG_DENS = 1.7                                                    322
         AG_TITLE = 'G10'                                                 322
         AG_EXNAME = 'MIXTURE'                                            322
         AG_PARLIST = 'DENS'                                              322
         CALL AXMIXTURE                                                   322
C    *                                                                    322
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      322
         CALL AGSRESET                                                    323
         AG_RMIN = 0                                                      323
         AG_RMAX = 0                                                      323
         AG_DZ = 0                                                        323
         AG_TITLE = 'TUBE'                                                323
         AG_EXNAME = 'SHAPE'                                              323
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      323
         CALL AXSHAPE                                                     323
C    *                                                                    323
5471  CALL AGSPOPB                                                        324
      GO TO BLOCK_BCEL, (5470 ,5480)                                      324
C End of Local Procedure                                                  324
C ----------------------------------------------------------------------  325
C ------------ BFEE                                                       325
C Begin Local Procedure BFEE                                              326
C BFEE IS A G10 DISCRIMINATOR/CW CONTROL BOARD FOR TOF                    326
5300  CONTINUE                                                            326
         AG_TITLE='BFEE'                                                  326
         AG_MODULE=MODULE                                                 326
         AG_MARK='V00'                                                    326
         CALL AGSPUSH                                                     326
         AG_MODULE=MODULE                                                 326
C    *                                                                    326
C    Geant ATTRIBUTE : BFEE ===> SEEN=1 COLO=3 <===                       327
         CALL AGSRESET                                                    327
         AG_SEEN = 1                                                      327
         AG_COLO = 3                                                      327
         AG_TITLE = 'BFEE'                                                327
         AG_EXNAME = 'ATTRIBUTE'                                          327
         AG_PARLIST = 'SEEN_COLO'                                         327
         CALL AXATTRIBUTE                                                 327
C    *                                                                    327
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          327
         CALL AGSRESET                                                    328
         AG_A = 28.08                                                     328
         AG_Z = 14                                                        328
         AG_W = 0.6*1*28./60.                                             328
         AG_TITLE = 'SI'                                                  328
         AG_EXNAME = 'COMPONENT'                                          328
         AG_PARLIST = 'A_Z_W'                                             328
         CALL AXCOMPONENT                                                 328
C    *                                                                    328
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               328
         CALL AGSRESET                                                    329
         AG_A = 16                                                        329
         AG_Z = 8                                                         329
         AG_W = 0.6*2*16./60.                                             329
         AG_TITLE = 'O'                                                   329
         AG_EXNAME = 'COMPONENT'                                          329
         AG_PARLIST = 'A_Z_W'                                             329
         CALL AXCOMPONENT                                                 329
C    *                                                                    329
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              329
         CALL AGSRESET                                                    330
         AG_A = 12                                                        330
         AG_Z = 6                                                         330
         AG_W = 0.4*8*12./174.                                            330
         AG_TITLE = 'C'                                                   330
         AG_EXNAME = 'COMPONENT'                                          330
         AG_PARLIST = 'A_Z_W'                                             330
         CALL AXCOMPONENT                                                 330
C    *                                                                    330
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               330
         CALL AGSRESET                                                    331
         AG_A = 1                                                         331
         AG_Z = 1                                                         331
         AG_W = 0.4*14*1./174.                                            331
         AG_TITLE = 'H'                                                   331
         AG_EXNAME = 'COMPONENT'                                          331
         AG_PARLIST = 'A_Z_W'                                             331
         CALL AXCOMPONENT                                                 331
C    *                                                                    331
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              331
         CALL AGSRESET                                                    332
         AG_A = 16                                                        332
         AG_Z = 8                                                         332
         AG_W = 0.4*4*16./174.                                            332
         AG_TITLE = 'O'                                                   332
         AG_EXNAME = 'COMPONENT'                                          332
         AG_PARLIST = 'A_Z_W'                                             332
         CALL AXCOMPONENT                                                 332
C    *                                                                    332
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               332
         CALL AGSRESET                                                    333
         AG_DENS = 1.7                                                    333
         AG_TITLE = 'G10'                                                 333
         AG_EXNAME = 'MIXTURE'                                            333
         AG_PARLIST = 'DENS'                                              333
         CALL AXMIXTURE                                                   333
C    *                                                                    333
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           333
         CALL AGSRESET                                                    334
         AG_DX = 0                                                        334
         AG_DY = 0                                                        334
         AG_DZ = 0                                                        334
         AG_TITLE = 'BOX'                                                 334
         AG_EXNAME = 'SHAPE'                                              334
         AG_PARLIST = 'DX_DY_DZ'                                          334
         CALL AXSHAPE                                                     334
C    *                                                                    334
5301  CALL AGSPOPB                                                        335
      GO TO BLOCK_BFEE, (5300 ,5310)                                      335
C End of Local Procedure                                                  335
C ----------------------------------------------------------------------  336
C ------------ BCOO                                                       336
C Begin Local Procedure BCOO                                              337
C BCOO ARE THE COOLING RAILS/LOOPS                                        337
5330  CONTINUE                                                            337
         AG_TITLE='BCOO'                                                  337
         AG_MODULE=MODULE                                                 337
         AG_MARK='V00'                                                    337
         CALL AGSPUSH                                                     337
         AG_MODULE=MODULE                                                 337
C    *                                                                    337
C    Geant ATTRIBUTE : BCOO ===> SEEN=1 COLO=2 <===                       338
         CALL AGSRESET                                                    338
         AG_SEEN = 1                                                      338
         AG_COLO = 2                                                      338
         AG_TITLE = 'BCOO'                                                338
         AG_EXNAME = 'ATTRIBUTE'                                          338
         AG_PARLIST = 'SEEN_COLO'                                         338
         CALL AXATTRIBUTE                                                 338
C    *                                                                    338
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           338
         CALL AGSRESET                                                    339
         AG_DX = 0                                                        339
         AG_DY = 0                                                        339
         AG_DZ = 0                                                        339
         AG_TITLE = 'BOX'                                                 339
         AG_EXNAME = 'SHAPE'                                              339
         AG_PARLIST = 'DX_DY_DZ'                                          339
         CALL AXSHAPE                                                     339
C    *                                                                    339
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               339
         AG_DX = TOFF_RAILTHCK/2                                          340
         AG_DY = TOFF_RAILWID/2                                           340
C    Create BRAI :                                                        340
C    Call To Local Procedure BRAI                                         340
         ASSIGN 5510 TO BLOCK_BRAI                                        340
         GO TO  5500                                                      340
5510     CONTINUE                                                         340
C    *                                                                    340
C    Geant POSITION : BRAI ===> X=TOFF_ELECX-TOFF_RAILTHCK Y=(TRAY_WIDTH  342
C    /2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                    342
         CALL AGSRESET                                                    342
         AG_X = TOFF_ELECX-TOFF_RAILTHCK                                  342
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                342
         AG_KONLY = 'MANY'                                                342
         AG_TITLE = 'BRAI'                                                342
         AG_EXNAME = 'POSITION'                                           342
         AG_PARLIST = 'X_Y_KONLY'                                         342
         CALL AXPOSITION                                                  342
C    *                                                                    342
C    Geant POSITION : BRAI ===> X=TOFF_ELECX-TOFF_RAILWID/2-TOFF_RAILTHC  342
C    K/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY='  342
C    *MANY' <===                                                          342
         CALL AGSRESET                                                    344
         AG_X = TOFF_ELECX-TOFF_RAILWID/2-TOFF_RAILTHCK/2                 344
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               344
         AG_ALPHAN=AG_ALPHAN+1                                            344
         AG_ALPHAK(AG_ALPHAN)=3                                           344
         AG_ALPHAL(AG_ALPHAN)=90                                          344
         AG_KONLY = 'MANY'                                                344
         AG_TITLE = 'BRAI'                                                344
         AG_EXNAME = 'POSITION'                                           344
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  344
         CALL AXPOSITION                                                  344
C    *                                                                    344
C    Geant POSITION : BRAI ===> X=TOFF_ELECX-TOFF_RAILTHCK Y=-(TRAY_WIDT  344
C    H/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                   344
         CALL AGSRESET                                                    346
         AG_X = TOFF_ELECX-TOFF_RAILTHCK                                  346
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               346
         AG_KONLY = 'MANY'                                                346
         AG_TITLE = 'BRAI'                                                346
         AG_EXNAME = 'POSITION'                                           346
         AG_PARLIST = 'X_Y_KONLY'                                         346
         CALL AXPOSITION                                                  346
C    *                                                                    346
C    Geant POSITION : BRAI ===> X=TOFF_ELECX-TOFF_RAILWID/2-TOFF_RAILTHC  346
C    K/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY=  346
C    'MANY' <===                                                          346
         CALL AGSRESET                                                    348
         AG_X = TOFF_ELECX-TOFF_RAILWID/2-TOFF_RAILTHCK/2                 348
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              348
         AG_ALPHAN=AG_ALPHAN+1                                            348
         AG_ALPHAK(AG_ALPHAN)=3                                           348
         AG_ALPHAL(AG_ALPHAN)=90                                          348
         AG_KONLY = 'MANY'                                                348
         AG_TITLE = 'BRAI'                                                348
         AG_EXNAME = 'POSITION'                                           348
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  348
         CALL AXPOSITION                                                  348
C    *                                                                    348
         AG_RMAX = TOFF_COOLOUTR                                          348
         AG_RMIN = TOFF_COOLINNR                                          349
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               349
C    Create BPIP :                                                        349
C    Call To Local Procedure BPIP                                         349
         ASSIGN 5530 TO BLOCK_BPIP                                        349
         GO TO  5520                                                      349
5530     CONTINUE                                                         349
C    *                                                                    349
C    Geant POSITION : BPIP ===> X=TOFF_ELECX-3.*TOFF_RAILTHCK/2.-TOFF_CO  351
C    OLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR) KO  351
C    NLY='MANY' <===                                                      351
         CALL AGSRESET                                                    351
         AG_X = TOFF_ELECX-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR              351
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   351
         AG_KONLY = 'MANY'                                                351
         AG_TITLE = 'BPIP'                                                351
         AG_EXNAME = 'POSITION'                                           351
         AG_PARLIST = 'X_Y_KONLY'                                         351
         CALL AXPOSITION                                                  351
C    *                                                                    351
C    Geant POSITION : BPIP ===> X=TOFF_ELECX-3.*TOFF_RAILTHCK/2.-TOFF_CO  351
C    OLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR) K  351
C    ONLY='MANY' <===                                                     351
         CALL AGSRESET                                                    353
         AG_X = TOFF_ELECX-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR              353
         AG_Y = -(TRAY_WIDTH/                                             353
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      353
         AG_KONLY = 'MANY'                                                353
         AG_TITLE = 'BPIP'                                                353
         AG_EXNAME = 'POSITION'                                           353
         AG_PARLIST = 'X_Y_KONLY'                                         353
         CALL AXPOSITION                                                  353
C    *                                                                    353
5331  CALL AGSPOPB                                                        354
      GO TO BLOCK_BCOO, (5330 ,5340)                                      354
C End of Local Procedure                                                  354
C ----------------------------------------------------------------------  355
C ------------ BRAI                                                       355
C Begin Local Procedure BRAI                                              356
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   356
5500  CONTINUE                                                            356
         AG_TITLE='BRAI'                                                  356
         AG_MODULE=MODULE                                                 356
         AG_MARK='V00'                                                    356
         CALL AGSPUSH                                                     356
         AG_MODULE=MODULE                                                 356
C    *                                                                    356
C    Geant ATTRIBUTE : BRAI ===> SEEN=1 COLO=1 <===                       357
         CALL AGSRESET                                                    357
         AG_SEEN = 1                                                      357
         AG_COLO = 1                                                      357
         AG_TITLE = 'BRAI'                                                357
         AG_EXNAME = 'ATTRIBUTE'                                          357
         AG_PARLIST = 'SEEN_COLO'                                         357
         CALL AXATTRIBUTE                                                 357
C    *                                                                    357
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        357
         CALL AGSRESET                                                    358
         AG_TITLE = 'ALUMINIUM'                                           358
         AG_EXNAME = 'MATERIAL'                                           358
         AG_PARLIST = 'NONE'                                              358
         CALL AXMATERIAL                                                  358
C    *                                                                    358
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     358
         CALL AGSRESET                                                    359
         AG_DX = 0.0                                                      359
         AG_DY = 0.0                                                      359
         AG_DZ = 0.0                                                      359
         AG_TITLE = 'BOX'                                                 359
         AG_EXNAME = 'SHAPE'                                              359
         AG_PARLIST = 'DX_DY_DZ'                                          359
         CALL AXSHAPE                                                     359
C    *                                                                    359
5501  CALL AGSPOPB                                                        360
      GO TO BLOCK_BRAI, (5500 ,5510)                                      360
C End of Local Procedure                                                  360
C ----------------------------------------------------------------------  361
C ------------ BPIP                                                       361
C Begin Local Procedure BPIP                                              362
C BPIP IS THE PIPE FOR THE COOLING LOOP                                   362
5520  CONTINUE                                                            362
         AG_TITLE='BPIP'                                                  362
         AG_MODULE=MODULE                                                 362
         AG_MARK='V00'                                                    362
         CALL AGSPUSH                                                     362
         AG_MODULE=MODULE                                                 362
C    *                                                                    362
C    Geant ATTRIBUTE : BPIP ===> SEEN=1 COLO=1 <===                       363
         CALL AGSRESET                                                    363
         AG_SEEN = 1                                                      363
         AG_COLO = 1                                                      363
         AG_TITLE = 'BPIP'                                                363
         AG_EXNAME = 'ATTRIBUTE'                                          363
         AG_PARLIST = 'SEEN_COLO'                                         363
         CALL AXATTRIBUTE                                                 363
C    *                                                                    363
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        363
         CALL AGSRESET                                                    364
         AG_TITLE = 'ALUMINIUM'                                           364
         AG_EXNAME = 'MATERIAL'                                           364
         AG_PARLIST = 'NONE'                                              364
         CALL AXMATERIAL                                                  364
C    *                                                                    364
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      364
         CALL AGSRESET                                                    365
         AG_RMIN = 0                                                      365
         AG_RMAX = 0                                                      365
         AG_DZ = 0                                                        365
         AG_TITLE = 'TUBE'                                                365
         AG_EXNAME = 'SHAPE'                                              365
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      365
         CALL AXSHAPE                                                     365
C    *                                                                    365
5521  CALL AGSPOPB                                                        366
      GO TO BLOCK_BPIP, (5520 ,5530)                                      366
C End of Local Procedure                                                  366
C ----------------------------------------------------------------------  367
C ------------ BUND                                                       367
C Begin Local Procedure BUND                                              368
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          368
5130  CONTINUE                                                            368
         AG_TITLE='BUND'                                                  368
         AG_MODULE=MODULE                                                 368
         AG_MARK='V00'                                                    368
         CALL AGSPUSH                                                     368
         AG_MODULE=MODULE                                                 368
C    *                                                                    368
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              369
         CALL AGSRESET                                                    369
         AG_SEEN = 0                                                      369
         AG_COLO = 1                                                      369
         AG_SERIAL = 0                                                    369
         AG_TITLE = 'BUND'                                                369
         AG_EXNAME = 'ATTRIBUTE'                                          369
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  369
         CALL AXATTRIBUTE                                                 369
C    *                                                                    369
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  369
C    SUPLEN/2 <===                                                        369
         CALL AGSRESET                                                    371
         AG_DX = TRAY_SUPFULLH/2                                          371
         AG_DY = TRAY_WIDTH/2                                             371
         AG_DZ = TRAY_SUPLEN/2                                            371
         AG_TITLE = 'BOX'                                                 371
         AG_EXNAME = 'SHAPE'                                              371
         AG_PARLIST = 'DX_DY_DZ'                                          371
         CALL AXSHAPE                                                     371
C    *                                                                    371
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        371
         CALL AGSRESET                                                    372
         AG_TITLE = 'ALUMINIUM'                                           372
         AG_EXNAME = 'MATERIAL'                                           372
         AG_PARLIST = 'NONE'                                              372
         CALL AXMATERIAL                                                  372
C    *                                                                    372
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           372
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             374
C    Create BTFT :                                                        375
C    Call To Local Procedure BTFT                                         375
         ASSIGN 5550 TO BLOCK_BTFT                                        375
         GO TO  5540                                                      375
5550     CONTINUE                                                         375
C    *                                                                    375
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  376
C    _FOOT1LEN/2 <===                                                     376
         CALL AGSRESET                                                    376
         AG_X = XPOS                                                      376
         AG_Y = -YPOS                                                     376
         AG_DX = TRAY_FOOTTHK/2                                           376
         AG_DY = TRAY_FOOT1LEN/2                                          376
         AG_TITLE = 'BTFT'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'X_Y_DX_DY'                                         376
         CALL AXPOSITION                                                  376
C    *                                                                    376
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  376
C    _FOOT1LEN/2 <===                                                     376
         CALL AGSRESET                                                    377
         AG_X = XPOS                                                      377
         AG_Y = +YPOS                                                     377
         AG_DX = TRAY_FOOTTHK/2                                           377
         AG_DY = TRAY_FOOT1LEN/2                                          377
         AG_TITLE = 'BTFT'                                                377
         AG_EXNAME = 'POSITION'                                           377
         AG_PARLIST = 'X_Y_DX_DY'                                         377
         CALL AXPOSITION                                                  377
C    *                                                                    377
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          377
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   379
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  380
C    Y_FOOTTHK/2 <===                                                     380
         CALL AGSRESET                                                    380
         AG_X = XPOS                                                      380
         AG_Y = -YPOS                                                     380
         AG_DX = TRAY_FOOT2THK/2                                          380
         AG_DY = TRAY_FOOTTHK/2                                           380
         AG_TITLE = 'BTFT'                                                380
         AG_EXNAME = 'POSITION'                                           380
         AG_PARLIST = 'X_Y_DX_DY'                                         380
         CALL AXPOSITION                                                  380
C    *                                                                    380
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  380
C    Y_FOOTTHK/2 <===                                                     380
         CALL AGSRESET                                                    381
         AG_X = XPOS                                                      381
         AG_Y = +YPOS                                                     381
         AG_DX = TRAY_FOOT2THK/2                                          381
         AG_DY = TRAY_FOOTTHK/2                                           381
         AG_TITLE = 'BTFT'                                                381
         AG_EXNAME = 'POSITION'                                           381
         AG_PARLIST = 'X_Y_DX_DY'                                         381
         CALL AXPOSITION                                                  381
C    *                                                                    381
         XPOS =(TRAY_SUPFULLH + TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           381
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   383
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  384
C    _FOOT3LEN/2 <===                                                     384
         CALL AGSRESET                                                    384
         AG_X = XPOS                                                      384
         AG_Y = -YPOS                                                     384
         AG_DX = TRAY_FOOTTHK/2                                           384
         AG_DY = TRAY_FOOT3LEN/2                                          384
         AG_TITLE = 'BTFT'                                                384
         AG_EXNAME = 'POSITION'                                           384
         AG_PARLIST = 'X_Y_DX_DY'                                         384
         CALL AXPOSITION                                                  384
C    *                                                                    384
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  384
C    _FOOT3LEN/2 <===                                                     384
         CALL AGSRESET                                                    385
         AG_X = XPOS                                                      385
         AG_Y = +YPOS                                                     385
         AG_DX = TRAY_FOOTTHK/2                                           385
         AG_DY = TRAY_FOOT3LEN/2                                          385
         AG_TITLE = 'BTFT'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_DX_DY'                                         385
         CALL AXPOSITION                                                  385
C    *                                                                    385
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              385
     *   TAN(60*DEGRAD)                                                   385
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           387
     *   SUPPORT_AILE_WIDTH                                               387
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        388
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               389
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          390
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        391
         CALL AGSRESET                                                    391
         AG_TITLE = 'ALUMINIUM'                                           391
         AG_EXNAME = 'MATERIAL'                                           391
         AG_PARLIST = 'NONE'                                              391
         CALL AXMATERIAL                                                  391
C    *                                                                    391
C    Create BASE :                                                        392
C    Call To Local Procedure BASE                                         392
         ASSIGN 5570 TO BLOCK_BASE                                        392
         GO TO  5560                                                      392
5570     CONTINUE                                                         392
C    *                                                                    392
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   393
         CALL AGSRESET                                                    393
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          393
         AG_TITLE = 'BASE'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'X'                                                 393
         CALL AXPOSITION                                                  393
C    *                                                                    393
C    Create BARM :                                                        394
C    Call To Local Procedure BARM                                         394
         ASSIGN 5590 TO BLOCK_BARM                                        394
         GO TO  5580                                                      394
5590     CONTINUE                                                         394
C    *                                                                    394
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  395
C    ===                                                                  395
         CALL AGSRESET                                                    395
         AG_X = SUPPORT_ARM_XPOS                                          395
         AG_Y = -SUPPORT_ARM_YPOS                                         395
         AG_TITLE = 'BARM'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'X_Y'                                               395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  395
C    ===                                                                  395
         CALL AGSRESET                                                    396
         AG_X = SUPPORT_ARM_XPOS                                          396
         AG_Y = +SUPPORT_ARM_YPOS                                         396
         AG_TITLE = 'BARM'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y'                                               396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Create BANG :                                                        397
C    Call To Local Procedure BANG                                         397
         ASSIGN 5610 TO BLOCK_BANG                                        397
         GO TO  5600                                                      397
5610     CONTINUE                                                         397
C    *                                                                    397
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 398
         CALL AGSRESET                                                    398
         AG_Y = -SUPPORT_AILE_YPOS                                        398
         AG_TITLE = 'BANG'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'Y'                                                 398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      398
         CALL AGSRESET                                                    399
         AG_Y = +SUPPORT_AILE_YPOS                                        399
         AG_THETAX = 270                                                  399
         AG_TITLE = 'BANG'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'Y_THETAX'                                          399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Create BCOV :                                                        400
C    Call To Local Procedure BCOV                                         400
         ASSIGN 5630 TO BLOCK_BCOV                                        400
         GO TO  5620                                                      400
5630     CONTINUE                                                         400
C    *                                                                    400
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     401
         CALL AGSRESET                                                    401
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            401
         AG_TITLE = 'BCOV'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'X'                                                 401
         CALL AXPOSITION                                                  401
C    *                                                                    401
5131  CALL AGSPOPB                                                        402
      GO TO BLOCK_BUND, (5130 ,5140)                                      402
C End of Local Procedure                                                  402
C ----------------------------------------------------------------------  403
C ------------ BTFT                                                       403
C Begin Local Procedure BTFT                                              404
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       404
5540  CONTINUE                                                            404
         AG_TITLE='BTFT'                                                  404
         AG_MODULE=MODULE                                                 404
         AG_MARK='V00'                                                    404
         CALL AGSPUSH                                                     404
         AG_MODULE=MODULE                                                 404
C    *                                                                    404
C    Geant ATTRIBUTE : BTFT ===> SEEN=1 COLO=2 <===                       405
         CALL AGSRESET                                                    405
         AG_SEEN = 1                                                      405
         AG_COLO = 2                                                      405
         AG_TITLE = 'BTFT'                                                405
         AG_EXNAME = 'ATTRIBUTE'                                          405
         AG_PARLIST = 'SEEN_COLO'                                         405
         CALL AXATTRIBUTE                                                 405
C    *                                                                    405
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     405
         CALL AGSRESET                                                    406
         AG_DX = 0.0                                                      406
         AG_DY = 0.0                                                      406
         AG_DZ = 0.0                                                      406
         AG_TITLE = 'BOX'                                                 406
         AG_EXNAME = 'SHAPE'                                              406
         AG_PARLIST = 'DX_DY_DZ'                                          406
         CALL AXSHAPE                                                     406
C    *                                                                    406
5541  CALL AGSPOPB                                                        407
      GO TO BLOCK_BTFT, (5540 ,5550)                                      407
C End of Local Procedure                                                  407
C ----------------------------------------------------------------------  408
C ------------ BARM                                                       408
C Begin Local Procedure BARM                                              409
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              409
5580  CONTINUE                                                            409
         AG_TITLE='BARM'                                                  409
         AG_MODULE=MODULE                                                 409
         AG_MARK='V00'                                                    409
         CALL AGSPUSH                                                     409
         AG_MODULE=MODULE                                                 409
C    *                                                                    409
C    Geant ATTRIBUTE : BARM ===> SEEN=1 COLO=2 <===                       410
         CALL AGSRESET                                                    410
         AG_SEEN = 1                                                      410
         AG_COLO = 2                                                      410
         AG_TITLE = 'BARM'                                                410
         AG_EXNAME = 'ATTRIBUTE'                                          410
         AG_PARLIST = 'SEEN_COLO'                                         410
         CALL AXATTRIBUTE                                                 410
C    *                                                                    410
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  410
C    =                                                                    410
         CALL AGSRESET                                                    411
         AG_DX = TRAY_SUPARMT/2                                           411
         AG_DY = SUPPORT_ARM_WIDTH/2                                      411
         AG_TITLE = 'BOX'                                                 411
         AG_EXNAME = 'SHAPE'                                              411
         AG_PARLIST = 'DX_DY'                                             411
         CALL AXSHAPE                                                     411
C    *                                                                    411
5581  CALL AGSPOPB                                                        412
      GO TO BLOCK_BARM, (5580 ,5590)                                      412
C End of Local Procedure                                                  412
C ----------------------------------------------------------------------  413
C ------------ BANG                                                       413
C Begin Local Procedure BANG                                              414
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                414
5600  CONTINUE                                                            414
         AG_TITLE='BANG'                                                  414
         AG_MODULE=MODULE                                                 414
         AG_MARK='V00'                                                    414
         CALL AGSPUSH                                                     414
         AG_MODULE=MODULE                                                 414
C    *                                                                    414
C    Geant ATTRIBUTE : BANG ===> SEEN=1 COLO=2 <===                       415
         CALL AGSRESET                                                    415
         AG_SEEN = 1                                                      415
         AG_COLO = 2                                                      415
         AG_TITLE = 'BANG'                                                415
         AG_EXNAME = 'ATTRIBUTE'                                          415
         AG_PARLIST = 'SEEN_COLO'                                         415
         CALL AXATTRIBUTE                                                 415
C    *                                                                    415
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   415
C    ALPH=-60 THET=0 PHI=0 <===                                           415
         CALL AGSRESET                                                    417
         AG_DX = TRAY_SUPARMT/2                                           417
         AG_DY = SUPPORT_AILE_WIDTH/2                                     417
         AG_ALPH = -60                                                    417
         AG_THET = 0                                                      417
         AG_PHI = 0                                                       417
         AG_TITLE = 'PARA'                                                417
         AG_EXNAME = 'SHAPE'                                              417
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               417
         CALL AXSHAPE                                                     417
C    *                                                                    417
5601  CALL AGSPOPB                                                        418
      GO TO BLOCK_BANG, (5600 ,5610)                                      418
C End of Local Procedure                                                  418
C ----------------------------------------------------------------------  419
C ------------ BASE                                                       419
C Begin Local Procedure BASE                                              420
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               420
5560  CONTINUE                                                            420
         AG_TITLE='BASE'                                                  420
         AG_MODULE=MODULE                                                 420
         AG_MARK='V00'                                                    420
         CALL AGSPUSH                                                     420
         AG_MODULE=MODULE                                                 420
C    *                                                                    420
C    Geant ATTRIBUTE : BASE ===> SEEN=1 COLO=2 <===                       421
         CALL AGSRESET                                                    421
         AG_SEEN = 1                                                      421
         AG_COLO = 2                                                      421
         AG_TITLE = 'BASE'                                                421
         AG_EXNAME = 'ATTRIBUTE'                                          421
         AG_PARLIST = 'SEEN_COLO'                                         421
         CALL AXATTRIBUTE                                                 421
C    *                                                                    421
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    421
         CALL AGSRESET                                                    422
         AG_DX = TRAY_SUPBASET/2                                          422
         AG_DY = TRAY_SUPBASEW/2                                          422
         AG_TITLE = 'BOX'                                                 422
         AG_EXNAME = 'SHAPE'                                              422
         AG_PARLIST = 'DX_DY'                                             422
         CALL AXSHAPE                                                     422
C    *                                                                    422
5561  CALL AGSPOPB                                                        423
      GO TO BLOCK_BASE, (5560 ,5570)                                      423
C End of Local Procedure                                                  423
C ----------------------------------------------------------------------  424
C ------------ BCOV                                                       424
C Begin Local Procedure BCOV                                              425
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     425
5620  CONTINUE                                                            425
         AG_TITLE='BCOV'                                                  425
         AG_MODULE=MODULE                                                 425
         AG_MARK='V00'                                                    425
         CALL AGSPUSH                                                     425
         AG_MODULE=MODULE                                                 425
C    *                                                                    425
C    Geant ATTRIBUTE : BCOV ===> SEEN=1 COLO=2 <===                       426
         CALL AGSRESET                                                    426
         AG_SEEN = 1                                                      426
         AG_COLO = 2                                                      426
         AG_TITLE = 'BCOV'                                                426
         AG_EXNAME = 'ATTRIBUTE'                                          426
         AG_PARLIST = 'SEEN_COLO'                                         426
         CALL AXATTRIBUTE                                                 426
C    *                                                                    426
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               426
         CALL AGSRESET                                                    427
         AG_RMIN = 0                                                      427
         AG_RMAX = TRAY_COOLOUTR                                          427
         AG_TITLE = 'TUBE'                                                427
         AG_EXNAME = 'SHAPE'                                              427
         AG_PARLIST = 'RMIN_RMAX'                                         427
         CALL AXSHAPE                                                     427
C    *                                                                    427
C    Create BWAT :                                                        428
C    Call To Local Procedure BWAT                                         428
         ASSIGN 5650 TO BLOCK_BWAT                                        428
         GO TO  5640                                                      428
5650     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : BWAT ===> inherited arguments used <===             429
         CALL AGSRESET                                                    429
         AG_TITLE = 'BWAT'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'NONE'                                              429
         CALL AXPOSITION                                                  429
C    *                                                                    429
5621  CALL AGSPOPB                                                        430
      GO TO BLOCK_BCOV, (5620 ,5630)                                      430
C End of Local Procedure                                                  430
C ----------------------------------------------------------------------  431
C ------------ BWAT                                                       431
C Begin Local Procedure BWAT                                              432
C BWAT IS TPC COOLING WATER                                               432
5640  CONTINUE                                                            432
         AG_TITLE='BWAT'                                                  432
         AG_MODULE=MODULE                                                 432
         AG_MARK='V00'                                                    432
         CALL AGSPUSH                                                     432
         AG_MODULE=MODULE                                                 432
C    *                                                                    432
C    Geant ATTRIBUTE : BWAT ===> SEEN=1 COLO=3 <===                       433
         CALL AGSRESET                                                    433
         AG_SEEN = 1                                                      433
         AG_COLO = 3                                                      433
         AG_TITLE = 'BWAT'                                                433
         AG_EXNAME = 'ATTRIBUTE'                                          433
         AG_PARLIST = 'SEEN_COLO'                                         433
         CALL AXATTRIBUTE                                                 433
C    *                                                                    433
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           433
         CALL AGSRESET                                                    434
         AG_A = 1                                                         434
         AG_Z = 1                                                         434
         AG_W = 2                                                         434
         AG_TITLE = 'H2'                                                  434
         AG_EXNAME = 'COMPONENT'                                          434
         AG_PARLIST = 'A_Z_W'                                             434
         CALL AXCOMPONENT                                                 434
C    *                                                                    434
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           434
         CALL AGSRESET                                                    435
         AG_A = 16                                                        435
         AG_Z = 8                                                         435
         AG_W = 1                                                         435
         AG_TITLE = 'O'                                                   435
         AG_EXNAME = 'COMPONENT'                                          435
         AG_PARLIST = 'A_Z_W'                                             435
         CALL AXCOMPONENT                                                 435
C    *                                                                    435
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             435
         CALL AGSRESET                                                    436
         AG_DENS = 1.0                                                    436
         AG_TITLE = 'WATER'                                               436
         AG_EXNAME = 'MIXTURE'                                            436
         AG_PARLIST = 'DENS'                                              436
         CALL AXMIXTURE                                                   436
C    *                                                                    436
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLINNR <===               436
         CALL AGSRESET                                                    437
         AG_RMIN = 0                                                      437
         AG_RMAX = TRAY_COOLINNR                                          437
         AG_TITLE = 'TUBE'                                                437
         AG_EXNAME = 'SHAPE'                                              437
         AG_PARLIST = 'RMIN_RMAX'                                         437
         CALL AXSHAPE                                                     437
C    *                                                                    437
5641  CALL AGSPOPB                                                        438
      GO TO BLOCK_BWAT, (5640 ,5650)                                      438
C End of Local Procedure                                                  438
      WRITE(LOUT,5660)'===>>  [End AgML Module BTOFGEO1]  <<==='          438
5660  FORMAT(A32,/,/)                                                     438
C MODULE BTOFGEO1                                                         440
      END                                                                 440
