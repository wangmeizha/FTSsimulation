/***************************************************************************
 *
 * $Id: StFmsPointPair.cxx,v 2.1 2015/09/14 16:15:50 ullrich Exp $
 *
 * Author: Akio Ogawa, Sep 2015
 ***************************************************************************
 *
 * Description: Implementation of StFmsPointPair
 *
 ***************************************************************************
 *
 * $Log: StFmsPointPair.cxx,v $
 * Revision 2.1  2015/09/14 16:15:50  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StFmsPointPair.h"
#include "StFmsPoint.h"
#include "StRoot/St_base/StMessMgr.h"
#include "TMath.h"

static const char rcsid[] = "$Id: StFmsPointPair.cxx,v 2.1 2015/09/14 16:15:50 ullrich Exp $";

StFmsPointPair::StFmsPointPair() : mFpsPid(0), mConeRadius{0.100, 0.070, 0.030}
{
    memset(mConeEnergy,0,sizeof(mConeEnergy));
}

StFmsPointPair::StFmsPointPair(StFmsPoint* p) : StFmsPointPair() {
    addPoint(p);
}

StFmsPointPair::StFmsPointPair(StFmsPoint* p1, StFmsPoint* p2) : StFmsPointPair() {
    addPoint(p1);
    addPoint(p2);
}

StFmsPointPair::~StFmsPointPair() { /* no op */ }

void StFmsPointPair::addPoint(StFmsPoint* p)  {
    mPoints.push_back(p);
    mFourMomentum = mFourMomentum + p->fourMomentum();
    mFpsPid += (p->fpsPid()/10)*pow(10,nPoints()-1);
}

StFmsPoint* StFmsPointPair::point(int v) {
    if (v>=0 && v<nPoints()) return mPoints[v];
    return 0;
}

Float_t StFmsPointPair::dgg() const {
    if (nPoints()!=2) return -1.0;
    return (mPoints[0]->XYZ() - mPoints[1]->XYZ()).mag();
}

Float_t StFmsPointPair::zgg() const {
    if (nPoints()!=2) return -1.0;
    float e1=mPoints[0]->energy();
    float e2=mPoints[1]->energy();
    return fabs(e1-e2)/(e1+e2);
}

Float_t StFmsPointPair::coneRadius(int cone) const {
    if (cone>=0 && cone<kFmsPointMaxCone) return mConeRadius[cone];
    return 0.0;
}

Float_t StFmsPointPair::coneEnergy(int cone) const {
    if (cone>=0 && cone<kFmsPointMaxCone) return mConeEnergy[cone];
    return 0.0;
}

Float_t StFmsPointPair::coneEnergyFraction(int cone) const {
    if (cone>=0 && cone<kFmsPointMaxCone && mConeEnergy[cone]>0.0) return energy()/mConeEnergy[cone];
    return -1.0;
}

void StFmsPointPair::setConeEnergy(int cone, float energy) {
    if (cone>=0 && cone<kFmsPointMaxCone)
        mConeEnergy[cone]=energy;
}

void StFmsPointPair::print(int opt) {
    cout << Form("StFmsPointPair: N=%3d E=%6.1f pT=%6.2f eta=%6.2f Zgg=%6.3f Dgg=%7.3f M=%6.3f ",
                 nPoints(),energy(),pT(),eta(),zgg(),dgg(),mass());
    for(int cone=0; cone<kFmsPointMaxCone; cone++) {
        cout << Form("R%d=%6.3f ", int(coneRadius(cone)*1000.0+0.5),coneEnergyFraction(cone));
    }
    cout << Form("PID=%d PointId: ",fpsPid());
    for(int i=0; i<nPoints(); i++) {
        cout << Form("%3d ",mPoints[i]->id());
    }
    cout << endl;
}
