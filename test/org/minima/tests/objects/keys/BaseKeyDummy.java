package org.minima.tests.objects.keys;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.BaseKey;
import org.minima.utils.digest.WinternitzOTSignature;

/**
 *
 * @author obucina
 */
public class BaseKeyDummy extends BaseKey {

    public BaseKeyDummy() {
        super();
    }

    @Override
    protected void initKeys(MiniData zPrivateSeed) {
        mLevel = MiniNumber.ONE;
        mMaxUses = MiniNumber.ONE;
        mUses = MiniNumber.ZERO;
        //Number of Bits of security
        mBitLength = new MiniNumber(zPrivateSeed.getLength() * 8);

        //Create a random seed
        mPrivateSeed = zPrivateSeed;

        //Create a WOTS
        WinternitzOTSignature wots = new WinternitzOTSignature(mPrivateSeed.getData(), getHashFunction(mBitLength), getWinternitz());

        //Get the Public Key..
        mPublicKey = new MiniData(wots.getPublicKey());
    }

    @Override
    public MiniData sign(MiniData zData) {
        return zData;
    }

    @Override
    public boolean verify(MiniData zData, MiniData zSignature) {
        return true;
    }
}
