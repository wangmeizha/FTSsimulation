/***************************************************************************
 *
 * $Id: StFtsCollection.cxx,v 2.6 2015/09/14 16:59:53 ullrich Exp $
 *
 * Author: Akio, Oct 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtsCollection.cxx,v $
 *
 **************************************************************************/
#include "StEvent/StFtsCollection.h"

#include "StEvent/StFtsHit.h"

static const char rcsid[] = "$Id: StFtsCollection.cxx,v 2.6 2015/09/14 16:59:53 ullrich Exp $";

StFtsCollection::StFtsCollection() {}

StFtsCollection::~StFtsCollection() {}

unsigned int StFtsCollection::numberOfHits() const {
    return mHits.size();
}

void StFtsCollection::addHit(StFtsHit* hit) {
    mHits.push_back(hit);
}

StSPtrVecFtsHit& StFtsCollection::hits() {
    return mHits;
}

const StSPtrVecFtsHit& StFtsCollection::hits() const {
    return mHits;
}

void StFtsCollection::print(int option) {
    cout << Form("NHit=%d\n",numberOfHits());
    if(option>=1) for(unsigned int i=0; i<numberOfHits(); i++) {hits()[i]->print(option);}
}

