/***************************************************************************
 *
 * $Id: StFtsCollection.h,v 2.6 2015/09/14 16:59:53 ullrich Exp $
 *
 * Author: Akio, Oct 2015
 *
 * $Log: StFtsCollection.h,v $
 *
 **************************************************************************/
#ifndef StFtsCollection_hh
#define StFtsCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"

class StFtsHit;

class StFtsCollection : public StObject {
public:
    StFtsCollection();
    ~StFtsCollection();
    
    void addHit(StFtsHit*);               // Add a hit to the collection    
    unsigned int numberOfHits() const;    // Return the number of hits in the collection    
    StSPtrVecFtsHit& hits();              // Return the hit list
    const StSPtrVecFtsHit& hits() const;
    
    void print(int option=0);
    
private:
    StSPtrVecFtsHit     mHits;      // Owns all FTS hits

    ClassDef(StFtsCollection, 0)
};

#endif
