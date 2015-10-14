/***************************************************************************
 *
 * $Id: StFtsHit.cxx,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: akio 2015 Oct
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************
 *
 * $Log: StFtsHit.cxx,v $
 *
 **************************************************************************/

#include "StFtsHit.h"
#include "StMessMgr.h"

void StFtsHit::print(int option){
    LOG_INFO << Form("D%1d XYZ=%8.3f %8.3f %8.3f dR/dPhi/dz=%8.3f %8.5f %8.3f dE=%8.3f",
		     StHit::hardwarePosition(),
		     mPosition.x(),mPosition.y(),mPosition.z(),
		     mPositionError.x(),mPositionError.y(),mPositionError.z(),
		     charge()
		     ) << endm;
}

ClassImp(StFtsHit);
