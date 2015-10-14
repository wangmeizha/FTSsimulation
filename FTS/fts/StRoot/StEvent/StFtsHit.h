/***************************************************************************
 *
 * $Id: StFtsHit.h,v 2.4 2013/04/24 17:27:43 ullrich Exp $
 * Author: Akio, Oct 2015
 *
 ***************************************************************************
 *
 * Description: data for individual ``hit'' on the FTS, i.e. a 1D cluster of strips
 *
 ***************************************************************************
 *
 * $Log: StFtsHit.h,v $
 *
 **************************************************************************/

#ifndef _ST_FTS_HIT_H_
#define _ST_FTS_HIT_H_

#include "StHit.h"

class StFtsHit : public StHit { 
public:
    StFtsHit() {};
    ~StFtsHit() {};

    virtual StDetectorId detector() const {return kFtsId;};   

    int  getDisc() const {return StHit::hardwarePosition();}
    void setDisc(int disc) {StHit::setHardwarePosition(disc);}

    void print(int option);

private:   
    //See StHit and StMeasuredPoint for variables, including mPosition and mPositionError and charge.
    //mPosition will be filled with xyz
    //mPositionError will be filled with dR,dPhi,dz
    
    ClassDef(StFtsHit,0);
}; 

#endif
