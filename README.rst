======
animan
======

I've had to create some DOM animations in the past where the jQuery animations flickered when moving and resizing two different (but related) DIVs. Manually editing the properties in the same function solved the problem. This is a small library to abstract the management of simple linear animations and the requisite calls to setTimeout.

Example
=======

The test page is in the repository.

Usage
=====

After including the script on your page, you'll have access to the following functions.
::

    animan.create
    animan.stop
    animat.stop_all

API
===

animan.create(steps, interval, callback)
----------------------------------------
::

    steps
        The number of steps to take in the animation. Your 'callback'
        will be called this many times.

    interval
        The speed in milliseconds between each step in the animation.

    callback
        This is the function where you do your manipulation. This
        function will be passed the following parameters:
            step
                The number of the current step, range of 0 - 'steps'
                passed to this function.

            steps
                The same as steps above, for your convenience.

            position
                This is a number, 0 - 1 (step / steps), representing the
                position in the current animation.

            animation
                This is an object with animation properties. It also
                contains some properties for internal use. You can use
                this to stop an animation in the middle of it's workings.
                For example:
                    if (something_you_care_about) {
                        animan.stop(animation);
                    }

animan.stop(animation)
----------------------
::

    animation
        An object created with animan.create.

animan.stop_all()
-----------------
All active animations are stopped.
