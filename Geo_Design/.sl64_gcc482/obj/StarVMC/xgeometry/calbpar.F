C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
******************************************************************************
* $Id: calbpar.g,v 1.1 2004/01/19 21:15:50 potekhin Exp $
* $Log: calbpar.g,v $
* Revision 1.1  2004/01/19 21:15:50  potekhin
* We need to make this sub a separate compilation unit due
* to necessary restructuring of the calb geometry
*
      SUBROUTINE CALBPAR(IMED,MEDIUM)                                     8
*
      INTEGER IMED                                                        10
      CHARACTER MEDIUM*(*)                                                11
*
C Check MEDIUM=='ABSORBER'                                                13
      IF (MEDIUM.EQ.'ABSORBER') THEN                                      13
* --- cuts for EMC absorber and scintillator
      CALL GSTPAR (IMED,'CUTGAM',0.00008)                                 15
      CALL GSTPAR (IMED,'CUTELE',0.001)                                   16
      CALL GSTPAR (IMED,'BCUTE' ,0.0001)                                  17
      ELSEIF (MEDIUM.EQ.'SENSITIVE') THEN                                 18
* --- cuts for SMD light material and gas
      CALL GSTPAR (IMED,'CUTGAM',0.00001)                                 20
      CALL GSTPAR (IMED,'CUTELE',0.00001)                                 21
      ENDIF                                                               22
*--- common cuts for hadrons and muons in EMC&SMD
      CALL GSTPAR (IMED,'CUTNEU',0.001)                                   26
      CALL GSTPAR (IMED,'CUTHAD',0.001)                                   27
      CALL GSTPAR (IMED,'CUTMUO',0.001)                                   28
*
      END                                                                 30
