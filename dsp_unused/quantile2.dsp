import("stdfaust.lib");

// Noise floor detector using running minimum with exponential decay
noiseFloorDetector(attackTime, releaseTime) = 
    abs : envelope(attackTime, releaseTime) : runningMin(releaseTime * 10)
with {
    // Envelope follower for level detection
    envelope(att, rel, x) = x : si.smooth(ba.tau2pole(att)) : si.smooth(ba.tau2pole(rel));
    
    // Running minimum with slow release
    runningMin(timeConstant, x) = loop ~ _
    with {
        loop(prev) = min(x, prev + (x - prev) * ba.tau2pole(timeConstant));
    };
};

// Alternative: Quantile-based noise floor estimation
quantileNoiseFloor(percentile, windowSize) = 
    _ : slidingBuffer(windowSize) : sortBuffer : selectQuantile(percentile)
with {
    slidingBuffer(n, x) = x : seq(i, n, @(i)) : par(i, n, _);
    sortBuffer = // Simplified - would need full sorting implementation
    selectQuantile(p, buffer) = buffer; // Select p-th percentile
};

// Simple noise floor detector using exponential minimum tracking
simpleNoiseFloor(releaseTime, x) = loop ~ _
with {
    level = abs(x);
    alpha = 1.0 - exp(-1.0 / (releaseTime * ma.SR));
    loop(prev) = select2(level < prev, prev * (1 - alpha * 0.1), level);
};

// Main processor with noise floor detection
process = _ <: _, (simpleNoiseFloor(5.0) : hbargraph("Noise Floor [unit:dB]", -80, -20));