import("stdfaust.lib");

NCh = 2;

process = ef.dryWetMixer(dryWet, variable_softclip);

dryWet = hslider("dryWet",100,0,100,1) /100;

variable_softclip(l,r) = l,r : par(i,2,gainup : function : gaindown : makeup) with {
    function = aa.softclipQuadratic2;
    gainup = _ * ba.db2linear(g);
    gaindown = _ * ba.db2linear(0-g);
    
    g = g_base - g_var;
    g_base = hslider("gain", -10,-40,40,1);

    g_var = l,r :> g_var_filter : comp with{
        
        g_var_filter = fi.highpass(4,hp_freq) : fi.lowpass(2,lp_freq) with{
            hp_freq = hslider("hp_freq[scale:log]",10,10,10000,1);
            lp_freq = hslider("lp_freq[scale:log]",60,10,10000,1);
        };
        
        comp = co.RMS_compression_gain_mono(strength,thresh,att,rel,knee,prePost) : ba.linear2db <: attach(_,hbargraph("env",-12,0)) with {
            strength = 0.8;
            thresh = hslider("g_var Thresh", -20,-40,0,1);
            att = 0.05;
            rel = 0.1;
            knee = 12;
            prePost = 0;
        };
    };
    

    makeup = _ * makeup_gain with {
        makeup_gain = hslider("makeup", -6,-20,20,0.5) : ba.db2linear;
    };
};



envelope = an.amp_follower_ud(0.01,0.1) : ba.linear2db <: attach(_,hbargraph("env",-70,0));