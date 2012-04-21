var jsdom = require('jsdom');
var request = require('request');
var url = require('url');
var cache = require('memory-cache');

exports.kepler = {};

exports.kepler.confirmed_planets = function (req, res) {
  var planets = null;
  jsdom.env('http://kepler.nasa.gov/Mission/discoveries/',
    ['http://code.jquery.com/jquery.min.js'],
    function (error, window) {
      var $ = window.jQuery;
      planets = [];

      $('#example tbody tr').each(function (index, row) {
        var $row = $(row);
        var $cols = $row.find('td');
        var numcols = $cols.length;
        var planet = null;

        var column_map = [
          'name',
          'koi_number',
          'jupiter_masses',
          'earth_masses',
          'jupited_radii',
          'earth_radii',
          'density',
          'temp',
          'transit_duration',
          'period',
          'semi_major_axis',
          'eccentricity',
          'inclination',
          'distance',
          'solar_temp',
          'solar_mass',
          'solar_radii',
          'solar_metallicity',
          'solar_right_ascension',
          'solar_declination',
          'solar_mag',
          'last_updated'];

        if (numcols == 22) {
          // It's a planetary row. Why 22? Just because. :(
          planet = {};

          $cols.each(function (index, col) {
            var $col = $(col);
            var val = $col.text().replace(/^[^\n]+\n/, '');

            if (index < column_map.length){
              planet[column_map[index]] = val;
            }
          });
        }

        if (planet) {
          planets.push(planet);
        }
      });

      if (!planets || planets.length < 1) {
        planets = null;
      }

      console.log('fetch_kepler_planets: Got planets.');
      console.log(planets);
      res.send(planets);
    }
  );
}
