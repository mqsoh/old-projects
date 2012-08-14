A layperson's guide to Kepler data.
===================================

.. _Back to the top:

Contents.
---------
- `Interesting columns in the exoplanets table`_.
- `Interesting columns in the Kepler candidates table`_.
- `Exoplanets table`_.
- `Kepler candidates table`_.

Introduction.
-------------

The following two tables are defined for an `API`_ provided at the `NASA
Exoplanet Archive`_. I am a layperson and a web developer. I recently
participated in the `International Space Apps Challenge`_ and wrote `a very
basic Kepler application`_. I'd like to write a better one, but I didn't
understand the data available to me. This is a list of the columns provided in
both tables with further explanations and links for more information (the
initial descriptions are provided by the `API`_ documentation). If a column has
no explanation, then I either considered it obvious or not related to the
information I need (I assume!).

What I would like to know about new planets is:

#. The position of, and distance to, the host star.
#. The planet's distance from its host star.
#. The mass of the planet compared to ours.
#. The size of the planet compared to ours.
#. The composition of the planet.
#. Whether or not the planet's orbit is in the "`habitable zone`_" of the host
   star. There's some marked criticism on the validity of the metric, though.

.. _Interesting columns in the exoplanets table:

Interesting columns in the exoplanets table.
--------------------------------------------

`Back to the top`_.

#. The position of, and distance to, the host star. My teammate in the space
   apps challenge used the right ascension, declination and distance to find the
   relative positions of the stars, but I'm intrigued by the Galactic_
   coordinate system (st_glon and st_glat).

   - st_dist
   - ra
   - dec
   - st_glon
   - st_glat

#. The planet's distance from its host star. We'll use the semi-major axis.

   - pl_orbsmax
   - pl_orbsmaxerr1
   - pl_orbsmaxerr2

#. The mass of the planet compared to ours.

   - pl_masse

#. The size of the planet compared to ours.

   - pl_rade

#. The composition of the planet. I know that there are some exoplanets for
   which this is known, but I don't think this can be determined with the
   provided data.

   - ?

#. Habitable zone. I think the stars temperature is determined by the spectral
   characteristics, so st_ssp (spectral type) is probably the source of the
   data. However, the table specifically has st_teff, the effective
   temperature. Is it derived from the spectral type, though?

   - st_ssp
   - st_ssperr
   - st_teff
   - st_tefferr
   - pl_orbsmax (where the planet is)
   - pl_orbsmaxerr1
   - pl_orbsmaxerr2

.. _Interesting columns in the Kepler candidates table:

Interesting columns in the Kepler candidates table.
---------------------------------------------------

`Back to the top`_.

#. The position of, and distance to, the host star. I don't see an equivalent
   to st_dist from the exoplanets table. There are normd* and classprob* fields
   that, if I understand the `Mahalonobis distance`_ wiki properly might be the
   sort of raw data that gets digested to the simple st_dist. Maybe the
   Galactic_ coordinates will be easier in this case.

   - ra
   - dec
   - glon
   - glat

#. The planet's distance from its host star.

   - sma (planet-star distance)
   - smaunc (uncertainty)

#. The mass of the planet compared to ours. I was surprised to find out that
   `Kepler can't determine the mass of the planet`_ at all. It's the follow up
   observations that provide that data, so we'll only be able to get this from
   confirmed exoplanets in the other table.

   - ?

#. The size of the planet compared to ours.

   - prad
   - pradunc

#. The composition of the planet. The eqt (equilibrium temperature) of the
   (surface of the) planet might suggest something.

   - ?

#. Habitable zone.

   - sma (planet-star distance)
   - smaunc
   - smasrad (planet-star distance to stellar radius) The stellar radius might
     suggest stellar class and thereby temperature, so this may have been meant
     for a habitability evaluation.
   - smasradun

.. _Exoplanets table:

Exoplanets table.
-----------------

`Back to the top`_

This is a table of confirmed exoplanets. This isn't limited to Kepler
discoveries, but since all Kepler discoveries start with 'Kepler', it can be
filtered if you (I) want. The following fields are available.


pl_hostname
    Planet host star name.

pl_letter
    Planet letter (b, c, d, ...).

hd_name
    HD_ name ::

        A cataloguing system.

hip_name
    HIP_ name ::

        A cataloguing system.

ra
    RA_ (deg) ::

        Right ascension. I may use the right ascension and declination to
        calculate the star's location in relation to us. However, the Galactic
        Coordinates look promising, too.

dec
    Dec_ (deg) ::

        Declination.

st_dist
    Distance (parsecs)

    - st_disterr
    - st_distlim
    - st_plxblend

st_vj
    `V (Johnson) magnitude`_ ::

        Classification of stars based on color.

    - st_vjerr
    - st_vjlim
    - st_vjblend

st_teff
    `Effective Temperature`_ (K) ::

        This is based on the class (color) of the star, so I may not use this.

    - st_tefferr
    - st_tefflim
    - st_teffblend

st_rad
    Stellar Radius (solar mass) ::

        Radius of the star as a multiple of the sun's radius.

    - st_raderr
    - st_radlim
    - st_radblend

st_mass
    Stellar Mass (solar radius) ::

        Mass of the star as a multiple of the sun's mass.

    - st_masserr
    - st_masslim
    - st_massblend

pl_orbper
    Period (days)

    - (+) pl_orbpererr1
    - pl_orbperlim

pl_orbsmax
    `Semi-Major Axis`_ (AU) ::

        In an ellipse, the distance from the center, through a focus, to the
        edge.

    - (+) pl_orbsmaxerr1
    - (-) pl_orbsmaxerr2
    - pl_orbsmaxlim

pl_orbincl
    Inclination_ (deg) ::

        There's an orbital plane for reference. The planet's orbital
        inclination is the angle between it's orbital plane and the reference
        plane.

    - (+) pl_orbinclerr1
    - (-) pl_orbinclerr2
    - pl_orbincllim

pl_orbtper
    Time of Periastron_ (Julian Days) ::

        The closest approach of the planet to its star. I'm not sure what this
        means in relation to days. Probably there's a day-zero point in
        describing an orbit. Anyway, the 'longitude of periastron' is below.

    - (+) pl_orbtpererr1
    - (-) pl_orbtpererr2
    - pl_orbtperlim

pl_orbeccen
    Eccentricity_ ::

        Amount by which the orbit deviates from a perfect circle.

    - (+) pl_orbeccenrerr1
    - (-) pl_orbeccenerr2
    - pl_orbeccenlim

pl_massj
    Planet Mass (Jupiter)

    - (+) pl_massjerr1
    - (-) pl_massjnerr2
    - pl_massjlim

pl_radj
    Planet Radius (Jupiter)

    - (+) pl_radjerr1
    - (-) pl_radjnerr2
    - pl_radjlim

pl_method
    discoveryMethod

st_glon
    Galactic_ Longitude (deg) ::

        Probably the easiest way to plot the star's position.

st_glat
    Galactic_ Latitude (deg)

st_elon
    Ecliptic_ Longitude (deg) ::

        Probably a more confusing way to plot the star's position since it
        relies on the path of the sun in the celestial sphere.

st_elat
    Ecliptic_ Latitude (deg)

st_plx
    Parallax_ (mas) ::

        This is a method of measuring distance. Probably st_dist is a result of
        this measurement.

    - (+) st_plxerr1
    - (-) st_plxerr2
    - st_plxlim
    - st_plxblend

st_pmra
    RA `Proper Motion`_ (mas/yr) ::

        I think that this is used to calculate the position of the star and
        that the galactic coordinates are probably a result of the calculation.

    - (+) st_pmraerr1
    - (-) st_pmraerr2
    - st_pmralim
    - st_pmrablend

st_pmdec
    Dec `Proper Motion`_ (mas/yr)

    - st_pmdecerr
    - st_pmdeclim
    - st_pmdecblend

st_pm
    `Proper Motion`_ (mas/yr)

    - st_pmerr
    - st_pmerrlim
    - st_pmerrblend

st_radv
    `Radial Velocity`_ ::

        I think this is the wobble of the star, but the value is a property of
        the orbiting body...) (km/sec)

    - st_radverr
    - st_radvlimn
    - st_radvblend

st_ssp
    `Spectral Type`_ ::

        The color of the star! Sweet!

    - st_ssperr
    - st_ssplim
    - st_sspblend

st_lum
    Luminosity (log solar luminosity) ::

        Maybe 'brightness' of the star... see Spectral Type.

    - st_lumerr
    - st_lumlim
    - st_lumblend

st_metfe
    [Fe/H] (dex) ::

        The metallicity of the star. I just saw on the Science Channel that any
        iron in a star causes the sun to go nova within minutes, so... :*

    - st_metfeerr
    - st_metfelim
    - st_metfeblend

st_vsini
    `V sin`_ (I) (km/sec) ::

        Something to do with the rotation of the star and line-of-sight.

    - st_vsinierr
    - st_vsinilim
    - st_vsiniblend

st_acts
    Stellar Activity Index (S-Index)

    - st_actserr
    - st_actslim
    - st_actsblend

st_actr
    Stellar Activity Log (RHK)

    - st_actrerr
    - st_actrlim
    - st_actrblend

st_actlx
    Stellar Activit (Lx)

    - st_actlxerr
    - st_actlxlim
    - st_actlxblend

st_nts
    Number of Light Curves ::

        I think the next seven fields are related to the number of measurements
        taken.

st_nplc
    Number non-HIP LCs

st_nglc
    Number HIP Light Curves

st_nrvc
    Number Radial Velocity Curves

st_naxa
    Number Amateur Light Curves

st_nimg
    Number of Images

st_nspec
    Number of Spectra

st_uj
    U `(Johnson) magnitude`_ ::

        Photometric system.

    - st_ujerr
    - st_ujlim
    - st_ujblend

st_bj
    B `(Johnson) magnitude`_

    - st_bjerr
    - st_bjlim
    - st_bjblend

st_rc
    R `(Cousins) magnitude`_ ::

        Photometric system.

    - st_rcerr
    - st_rclim
    - st_rcblend

st_ic
    I `(Cousins) magnitude`_

    - st_icerr
    - st_iclim
    - st_icblend

st_j
    J (2MASS_) magnitude ::

        Photometric system.

    - st_jerr
    - st_jlim
    - st_jblend

st_h
    H (2MASS_) magnitude

    - st_herr
    - st_hlim
    - st_hblend

st_k
    K (2MASS_) magnitude

    - st_kerr
    - st_klim
    - st_kblend

st_irac1
    IRAC 3.6 magnitude ::

        Photometric system.

    - st_irac1err
    - st_irac1lim
    - st_irac1blend

st_irac2
    IRAC 4.5 magnitude

    - st_irac2err
    - st_irac2lim
    - st_irac2blend

st_irac3
    IRAC 5.8 magnitude

    - st_irac3err
    - st_irac3lim
    - st_irac3blend

st_irac4
    IRAC 8.0 magnitude

    - st_irac4err
    - st_irac4lim
    - st_irac4blend

st_mips1
    MIPS 24 micron flux (Jy) ::

        Photometric system.

    - st_mips1err
    - st_mips1lim
    - st_mips1blend

st_mips2
    MIPS 70 micron flux (Jy)

    - st_mips2err
    - st_mips2lim
    - st_mips2blend

st_mips3
    MIPS 160 micron flux (Jy)

    - st_mips3err
    - st_mips3lim
    - st_mips3blend

st_iras1
    IRAS 12 micron flux (Jy) ::

        Photometric system.

    - st_iras1err
    - st_iras1lim
    - st_iras1blend

st_iras2
    IRAS 25 micron flux (Jy)

    - st_iras2err
    - st_iras2lim
    - st_iras2blend

st_iras3
    IRAS 60 micron flux (Jy)

    - st_iras3err
    - st_iras3lim
    - st_iras3blend

st_iras4
    IRAS 100 micron flux (Jy)

    - st_iras3err
    - st_iras3lim
    - st_iras3blend

st_umbj
    (U-B) color (mags) ::

        Photometric system.

    - st_umbjerr
    - st_umbjlim
    - st_umbjblend

st_bmvj
    (B-V) color (mags)

    - st_bmvjerr
    - st_bmvjlim
    - st_bmvjblend

st_vjmic
    (V-Ic) color (mags)

    - st_vjmicerr
    - st_vjmiclim
    - st_vjmicblend

st_vjmrc
    (V-Rc) color (mags)

    - st_vjmrcerr
    - st_vjmrclim
    - st_vjmrcblend

st_jmh2
    (J-H) color (mags)

    - st_jmh2err
    - st_jmh2lim
    - st_jmh2blend

st_hmk2
    (H-K) color (mags)

    - st_hmk2err
    - st_hmk2lim
    - st_hmk2blend

st_jmk2
    (J-K) color (mags)

    - st_jmk2err
    - st_jmk2lim
    - st_jmk2blend

st_bmy
    Stromgren (b-y) (mags) ::

        Photometric system.

    - st_bmyerr
    - st_bmylim
    - st_bmyblend

st_m1
    Stromgren m1 (mags)

    - st_m1err
    - st_m1lim
    - st_m1blend

st_c1
    Stromgren c1 (mags)

    - st_c1err
    - st_c1lim
    - st_c1blend

pl_orblper
    Longitude of Periastron_ (deg) ::

        The closest approach of the planet to its star. There's a pl_orbtper
        which is the time of Periastron. This is the location.

    - (+) pl_orblpererr1
    - (-) pl_orblpererr2
    - pl_orblperlim

pl_masse
    Planet Mass (Earth) ::

        Number of Earth masses in this planet. Come on, 1!

    - (+) pl_masseerr1
    - (-) pl_masseerr2
    - pl_masselim

pl_rade
    Planet Radius (Earth) ::

        Number of Earth radiuses in this planet. Does this matter if the mass
        is near-Earth? I'm thinking of tiny planets like Le Petit Prince. Or a
        big fluffy pillow planet...

    - (+) pl_radeerr1
    - (-) pl_radeerr2
    - pl_radelim

pl_rads
    Planet Radius (solar) ::

        Probably the number of sun-radiuses in the planets, but...that doesn't
        seem to be useful since the sun is quite large. Maybe there are planets
        that big? I think I have all I need with the other fields, so I'll
        ignore this.

    - (+) pl_radserr1
    - (-) pl_radserr2
    - pl_radserrlim

pl_tran
    Transit Flag (1=yes, 0=no) ::

        There has to be a transit to measure it in the first place. I don't
        know what this means.

pl_trandep
    Transit Depth (percentage)

    - (+) pl_trandeperr1
    - (-) pl_trandeperr2
    - pl_trandeplim

pl_trandur
    Transit Duration (days)

    - (+) pl_trandurerr1
    - (-) pl_trandurerr2
    - pl_trandurlim

pl_tranmid
    Transit Mid-point (Julian days)

    - (+) pl_tranmiderr1
    - (-) pl_tranmiderr2
    - pl_tranmidlim

pl_disc
    Discovery Year

pl_status
    Planet Status ::

        In this table, they're all 3. I think 3 is 'confirmed'.

pl_pelink
    Planet Encyclopedia ::

        Link to more data.

pl_edelink
    Exoplanet Data Explorer ::

        Link to more data.



.. _Kepler candidates table:

Kepler candidates table.
------------------------

`Back to the top`_.

kepid
    Unique Kepler Identifier

ra
    RA_ (deg) ::

        Right ascension, used for finding the star in the celestial sphere.

dec
    Dec_ (deg) ::

        Declination, used for finding the star in the celestial sphere.

kepmag
    Kepler-band Magnitude ::

        The only search results I found were people providing the same data. I
        guess it's just a measurement of the brightness of the star. The
        following site gave it's range as 2.982 to 25.0.

        http://archive.stsci.edu/kepler/kic10/help/columns.html

teff
    `Effective Temperature`_ (K) ::

        It's based on the class of the star.

logg
    `Surface Gravity`_ (cm/s?) ::

        The exoplanets table doesn't have this information for the Kepler
        planets. Maybe it's implicit in the mass/radius calculations.

radius
    Stellar Radius (solar radius) ::

        Radius of the host star as a multiple of the sun's radius.

mass
    Stellar Mass (solar mass) ::

        Mass of the host star as a multiple of the sun's mass.

stflag
    Flag for origin of stellar parameters:

    0: Teff,log(g), and Rad are derived using KIC J-K color and linear
    interpolation of luminosity class V stellar properties of Schmidt-Kaler
    (1982).

    1: KIC Teff and log(g) are used as initial values for MCMC parameter search
    of Yonsei-Yale stellar evolution models yielding Teff, log(g), and Rad.

    2: Teff, log(g), and Rad are derived using SPC spectral synthesis and
    interpolation of the Yale-Yonsei evolutionary tracks.

    3: Teff, log(g), and Rad are derived using SME spectral synthesis and
    interpolation of the Yale-Yonsei evolutionary tracks.

cdpp6
    Combined 6 hour differential photometric precision (rms of quarters 1
    through 6 in units of parts per million)

kepoi_name
    `Kepler object of interest`_ name for display (KNNNNN.DD) ::

        Catalog.

kepler_name
    Kepler name for confirmed planets (e.g. Kepler-6b)

kepoi_type
    KepOI Type (CANDIDATE, CANDIDATE-FOP (a candidate being studied by the
    Kepler Mission Follow-up Observing PRogram), CONFIRMED, FALSE POSITIVE) ::

        This is a planet status.

period
    Period

periodunc
    Period uncertainty (days) in BKJD=BJD-2454833

epoch
    Transit_ epoch (days) ::

        The values are more than 100 and less than 400. I think this is number
        of days it took for the planet to pass in front of the star, but 300+
        days seems like a lot.

        Perhaps the epoch is the days from a reference point, like the start of
        the mission.

epochunc
    Transit_ epoch uncertainty (days)

depth
    Transit_ depth (ppm) ::

        NASA's Kepler mission site gives this as "the fractional change in
        brightness". "Depth" probably refers to the dip in the light curve.

depthunc
    Transit_ depth uncertainty (ppm)

duration
    Transit_ duration

durationunc
    Transit_ duration uncertainty (hours)

impact
    Impact parameter ::

        My searches only return things about the social and scientific impact
        of the mission, not a description of what this measurement means.

impactunc
    Impact parameter uncertainty

occdp
    Occultation depth (Relative flux level at phase=0.5 divided by noise) ::

        Probably related to transit depth. I don't think I need this field.

occdpunc
    Occultation depth uncertainty

sma
    Planet-star distance (AU); note that is the semi-major axis when
    eccentricity = 0

smaunc
    Planet-star distance uncertainty (AU)

smasrad
    Planet-star distance to Stellar Radius Ratio ::

        This can probably be used to evaluate the habitable zone.

smasradun
    Planet-star distance to Stellar Radius Ratio uncertainty

pradsrad
    Planet to Stellar Radius Ratio ::

        This can probably be used to evaluate the habitable zone.

pradsradunc
    Planet to Stellar Radius Ratio uncertainty

prad
    Planet Radius (Earth radius) ::

        Radius of the planet as a multiple of Earth's radius.

pradunc
    Planet Radius (Earth radius) uncertainty

eqt
    Equilibrium temperature (K) ::

        On the wiki page for Kepler-11g, the equilibrium temperature is the
        surface temperature of the planet in the absence of atmospheric
        effects. Probably this is a function of the class of the host star and
        the planets orbital distance.

eqtunc
    Equilibrium temperature uncertainty (K)

tm_designation
    2MASS name

glon
    Galactic_ Longitude (deg)

glat
    Galactic_ Latitude (deg)

gmag
    g'-band magnitude

rmag
    r'-band magnitude

imag
    i'-band magnitude

zmag
    z'-band magnitude

gredmag
    GRED-band magnitude

d51mag
    D51-band magnitude

jmag
    J-band magnitude

hmag
    H-band magnitude

kmag
    K-band magnitude

grcolor
    (g'-r') color magnitude

jkcolor
    (J-K) color magnitude

gkcolor
    (g'-K) color magnitude

feh
    [Fe/H] (dex) ::

        Metallicity of the star.

ebminusv
    E(B-V) reddening (mag)

av
    Av extinction (mag)

vsini
    `V sin`_ (i) (km/sec)

parallax
    Parallax_ (arcsec) ::

        Method of measuring distance.

pmtotal
    `Proper Motion`_ (arcsec/year)

pmra
    RA `Proper Motion`_ (arcsec/year)

pmdec
    Dec `Proper Motion`_ (arcsec/year)

rv
    `Radial Velocity`_ (km/sec) ::

        The wobble of the star.

galaxy
    Star/Galaxy Flag (0=star, 1=galaxy) ::

        All the values in this table are 0. This is probably cruft from another
        database schema.

blend
    Blend Flag

variable
    Constant/Variable Flag (0=constant, 1=variable) ::

        All 0 in this table.

fov_flag
    FOV Flag (0=outside, FOV, 1=non-target, 2=target)

crowding
    Fraction of flux (target/total)

neb
    Number of Eclipsing Binaries

ncen
    Number of Centroid Values

nts
    Number of Time Series

nlc
    Number of Long Cadence

nsc
    Number of Short Cadence

normd1
    Normalized `Mahalonobis distance`_ to most probable class (class 1)

normd2
    Normalized `Mahalonobis distance`_ to second most probable class (class 2)

normd3
    Normalized `Mahalonobis distance`_ to third most probable class (class 3)

classprob1
    Relative probability for class 1 ::

        Presumably this refers to the planet, however, the only thing I could
        find is the Sudarsky extrasolar planet classification which actually
        has five classes. Maybe the Kepler candidates are only part of the
        first three classes somehow.

    `Sudarsky extrasolar planet classification`_.

classprob2
    Relative probability for class 2

classprob3
    Relative probability for class 3

classcode1
    Variability class 1

classcode2
    Variability class 2

classcode3
    Variability class 3

spf1
    Significance parameter frequency 1 (probability) ::

        Unproductive search for "significance astronomy" and "significance
        parameter frequency".

spf2
    Significance parameter frequency 2 (probability)

spf3
    Significance parameter frequency 3 (probability)

freq1
    Frequency 1 (cycles per day)

freq2
    Frequency 2 (cycles per day)

freq3
    Frequency 3 (cycles per day)

amp11
    Amplitude of 1st harmonic of frequency 1 (mags)

amp12
    Amplitude of 2nd harmonic of frequency 1 (mags)

amp13
    Amplitude of 3rd harmonic of frequency 1 (mags)

amp14
    Amplitude of 1st harmonic of frequency 2 (mags)

amp21
    Amplitude of 2nd harmonic of frequency 2 (mags)

amp22
    Amplitude of 3rd harmonic of frequency 2 (mags)

amp23
    Amplitude of 4th harmonic of frequency 2 (mags)

amp24
    Amplitude of 1st harmonic of frequency 3 (mags)

amp31
    Amplitude of 2nd harmonic of frequency 3 (mags)

amp32
    Amplitude of 3rd harmonic of frequency 3 (mags)

amp33
    Amplitude of 3rd harmonic of frequency 3 (mags)

amp34
    Amplitude of 4th harmonic of frequency 3 (mags)

phdiff12
    Phase of amp12, if phase if amp11=0 (radians)

phdiff13
    Phase of amp12, if phase if amp11=0 (radians)

phdiff14
    Phase of amp13, if phase if amp11=0 (radians)

phdiff21
    Phase of amp21, if phase if amp11=0 (radians)

phdiff22
    Phase of amp22, if phase if amp11=0 (radians)

phdiff23
    Phase of amp23, if phase if amp11=0 (radians)

phdiff24
    Phase of amp24, if phase if amp11=0 (radians)

phdiff31
    Phase of amp31, if phase if amp11=0 (radians)

phdiff32
    Phase of amp32, if phase if amp11=0 (radians)

phdiff33
    Phase of amp33, if phase if amp11=0 (radians)

phdiff34
    Phase of amp34, if phase if amp11=0 (radians)

varred
    Total variance_ reduction of the light curve (after fit subtraction) ::

        I think this is how well the plot matches that dip in the light wave
        that indicates a planet transit.

koi_flag
    KOI Flag designating single transit or large uncertainties. From Borucki et
    al (2011): dd = KOI was detected on the basis of a single transit with the
    period derived from the transit duration and stellar radius.

snr
    Signal to noise ratio

mes
    Multiple Event Statistic; MES is the detection statistic akin to a total
    SNR of the phase-folded transit but constructed using the matched filter
    correlation statistics over phase and period.

chi
    Goodness of fit metric

oeslc
    Ratio of odd to even numbered transit depths dervied from light curve
    modeling

oesdv
    Ratio of odd to even numbered transit depths reported by data validation
    pipeline

cenra
    Centroid_ RA_ offset (arcsec); transit source position minus target star
    position ::

        I think this means that Kepler's able to distinguish the distance to
        the planet and the distance to the star. That's one fine machine.
        Probably that's what all that parallax stuff is about.

cenraunc
    Centroid_ RA_ offset uncertainty (arcsec); transit source position minus
    target star position

cendec
    Centroid_ dec_ offset (arcsec); transit source position minus target star
    position

cendecunc
    Centroid_ dec_ offset uncertainty (arcsec); transit source position minus
    target star position

cenoffset
    Centroid_ total offset (arcsec); transit source position minus target star
    position

cenoffsetunc
    Centroid_ total offset uncertainty (arcsec); transit source position minus
    target star position

obs
    Observed quarters; Six integers indicating which quarters the star was
    observed. ::

        I found something that referred to quarters as a segment of time for
        observations.

dra
    RA Offset of background object containing the transit ::

        Is the background object the star?

ddec
    Dec Offset of background object containing the transit

offset
    Offset of background object containing the transit

bkgdepth
    Background Object Transit Depth

bkgkepid
    Background Object KepID

djmag
    Delta J-band magnitude between background object and target

scicomm
    Science team comment ::

        These are additional details about the observation like "Secondary
        eclipse" and "Stellar binary", not exclamations like "w00t!" or "Space
        is the place!". Wholly professional.


.. _NASA Exoplanet Archive: http://exoplanetarchive.ipac.caltech.edu/
.. _API: http://exoplanetarchive.ipac.caltech.edu/docs/program_interfaces.html
.. _International Space Apps Challenge: http://spaceappschallenge.org/
.. _HD: http://en.wikipedia.org/wiki/Henry_Draper_catalogue
.. _HIP: http://en.wikipedia.org/wiki/Hipparcos_Catalogue
.. _RA: http://en.wikipedia.org/wiki/Right_ascension
.. _Dec: http://en.wikipedia.org/wiki/Declination
.. _dec: http://en.wikipedia.org/wiki/Declination
.. _V (Johnson) magnitude: http://en.wikipedia.org/wiki/UBV_photometric_system
.. _Inclination: http://en.wikipedia.org/wiki/Inclination
.. _Periastron: http://en.wikipedia.org/wiki/Periastron
.. _Eccentricity: http://en.wikipedia.org/wiki/Eccentricity_%28orbit%29
.. _Galactic: http://en.wikipedia.org/wiki/Galactic_coordinate_system
.. _Ecliptic: http://en.wikipedia.org/wiki/Ecliptic_coordinate_system
.. _Parallax: http://en.wikipedia.org/wiki/Stellar_parallax
.. _Proper Motion: http://en.wikipedia.org/wiki/Proper_motion
.. _Radial Velocity: http://en.wikipedia.org/wiki/Radial_velocity
.. _Spectral Type: http://en.wikipedia.org/wiki/Spectral_type#Spectral_types
.. _V sin: http://books.google.com/books?id=jAe4P3GIZRoC&pg=PT117&lpg=PT117&dq=stellar+v+sin&source=bl&ots=y7LR1tuIn6&sig=hlhPvulvidjuBydFFHowme1f-lc&hl=en&sa=X&ei=6b2ZT4OWNaLH6QHjq6TlBg&ved=0CGkQ6AEwCQ#v=onepage&q=v%20sin&f=false
.. _(Johnson) magnitude: http://www.astro.utoronto.ca/~patton/astro/mags.html
.. _(Cousins) magnitude: http://www.astrophysicsspectator.com/topics/observation/MagnitudesAndColors.html
.. _2MASS: http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec6_4a.html
.. _Semi-Major Axis: http://en.wikipedia.org/wiki/Semi-major_axis
.. _Effective Temperature: http://en.wikipedia.org/wiki/Star#Temperature
.. _Surface Gravity: http://en.wikipedia.org/wiki/Surface_gravity
.. _Kepler object of interest: http://en.wikipedia.org/wiki/Kepler_Object_of_Interest
.. _Transit: http://kepler.nasa.gov/Science/about/characteristicsOfTransits/
.. _Mahalonobis distance: http://en.wikipedia.org/wiki/Mahalanobis_distance
.. _Sudarsky extrasolar planet classification: http://en.wikipedia.org/wiki/Sudarsky_extrasolar_planet_classification
.. _variance: http://en.wikipedia.org/wiki/Variance
.. _Centroid: http://en.wikipedia.org/wiki/Centroid
.. _habitable zone: http://en.wikipedia.org/wiki/Habitable_zone
.. _a very basic Kepler application: http://www.keplercompanion.org/
.. _Kepler can't determine the mass of the planet: http://www.scientificamerican.com/article.cfm?id=kepler-planets-700
