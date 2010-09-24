/**
 * Copyright © 2010 Mason Staugler <http://www.staugler.net>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
(function (win) {
    win.animan = win.animan ? win.animan : (function () {
        var that = this,
        active_animations = [];

        /**
         * Create a new animation.
         * -----------------------
         * steps
         *     The number of steps to take in the animation. Your 'callback'
         *     will be called this many times.
         *
         * interval
         *     The speed in milliseconds between each step in the animation.
         *
         * callback
         *     This is the function where you do your manipulation. This
         *     function will be passed the following parameters:
         *         step
         *             The number of the current step, range of 0 - 'steps'
         *             passed to this function.
         *
         *         steps
         *             The same as steps above, for your convenience.
         *
         *         position
         *             This is a number, 0 - 1 (step / steps), representing the
         *             position in the current animation.
         *
         *         animation
         *             This is an object with animation properties. It also
         *             contains some properties for internal use. You can use
         *             this to stop an animation in the middle of it's workings.
         *             For example:
         *                 if (something_you_care_about) {
         *                     animan.stop(animation);
         *                 }
         */
        that.create = function (steps, interval, callback) {
            var ani = {
                step: 0,
                steps: steps,
                interval: interval,
                callback: callback,
                timeout: null,
                index: null,
            };

            ani.index = active_animations.push(ani) - 1;

            // Always make the first step.
            that.step(ani);

            return ani;
        };

        /**
         * Stop an animation.
         * ------------------
         * animation
         *     An object created with animan.create.
         */
        that.stop = function (animation) {
            if (animation.timeout !== null) {
                clearTimeout(animation.timeout);
                animation.timeout = null;

                if (typeof active_animations[animation.index] !== 'undefined') {
                    delete active_animations[animation.index];
                }
            }
        };

        /**
         * Stop all animations.
         * --------------------
         * All active animations are stopped.
         */
        that.stop_all = function () {
            var ii;
            for (ii in active_animations) {
                that.stop(active_animations[ii]);
            }
            active_animations = [];
        };

        /**
         *  Step an animation.
         *  ------------------
         *  This is for internal use. This is where the timeouts are created
         *  and the callbacks executed.
         */
        that.step = function (animation) {
            if (animation.step > animation.steps) {
                that.stop(animation);
                return;
            }

            animation.callback(
                animation.step,
                animation.steps,
                animation.step / animation.steps,
                animation
            );

            animation.step = animation.step + 1;
            if (animation.step <= animation.steps) {
                if (animation.step === 1 || animation.timeout !== null) {
                    animation.timeout = setTimeout(function () { that.step(animation); }, animation.interval);
                }
            }
        };

        return that;
    })();
})(window);
