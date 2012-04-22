var request = require('request');
var cache = require('memory-cache');

exports.home = function (req, res) {
  res.render('index', {
    title: 'Home'
  });
};

exports.visualization = function (req, res) {
  res.render('visualization', {
    title: 'Visualization'
  });
};

exports.list = function (req, res) {
  // The following crap is copied from the API module because there's something
  // I must not understand about closures and I don't know how to make Node.JS
  // code modular.

  var cache_key = 'masons-holiday-destinations';
  var csv = 'http://exoplanets.org/exoplanets.csv';
  var stored = cache.get(cache_key);

  if (stored) {
    // This is different.
    res.render('list', {
      title: 'All confirmed exoplanets.',
      sorted_exoplanets: sort_teh_planets(stored)
    });
    return;
  }

  request(csv, function (error, response, body) {
    console.log('Fetching ' + csv);

    var planets = [];
    var planet = null;
    var column_names = null;

    body = body.replace('\r', '\n');

    var lines = body.split('\n');

    for (var ii in lines) {
      var line = lines[ii];

      if (line.length < 1) {
        continue;
      }

      if (ii == 0) {
        column_names = csv_columns(line);
        continue;
      }

      planet = {};
      var cols = csv_columns(line);
      for (var jj in cols) {
        var col = cols[jj];
        planet[column_names[jj].toLowerCase()] = col;
      }

      planets.push(planet);
    }

    cache.put(cache_key, planets, 24 * 60 * 60);

    // This is also different, but also the same in a way. Different from the
    // API module, but the same as above. :(
    res.send(stored);
    res.render('list', {
      title: 'All confirmed exoplanets.',
      sorted_exoplanets: sort_teh_planets(planets)
    });
  });
};

function sort_teh_planets(planets) {
  var mass_property = 'msini';
  var earth_mass_multiple = 0.00314;

  var radius_property = 'r';
  var earth_radius_multiple = 0.089;

  var sorted = [];

  // Only Kepler!
  for (var ii in planets) {
    var planet = planets[ii];

    if (planet.name.toLowerCase().indexOf('kepler') !== -1) {
      var filtered_planet = {};

      filtered_planet.name = planet.name;
      filtered_planet.date = new Date(planet.date);

      // I'm adding the u- (uncertainty) properties because there are a few
      // planets wherein the certain property is 0. I checked a couple against
      // the Kepler mission page's values and they're populated, so...I'll
      // assume that's okay. I didn't validate the value, though. I'm going on
      // 20 hours of working now.

      filtered_planet.mass_jupiter = parseFloat(planet[mass_property]) + parseFloat(planet['u'+mass_property]);
      filtered_planet.mass_earth = filtered_planet.mass_jupiter * earth_mass_multiple;

      filtered_planet.radius_jupiter = parseFloat(planet[radius_property]) + parseFloat(planet['u'+radius_property]);
      filtered_planet.radius_earth = filtered_planet.radius_jupiter * earth_radius_multiple;

      if (isNaN(filtered_planet.mass_jupiter)) {
        filtered_planet.mass_jupiter = '--';
        filtered_planet.mass_earth = '--';
      }
      else {
        filtered_planet.mass_jupiter = filtered_planet.mass_jupiter.toFixed(4);
        filtered_planet.mass_earth = filtered_planet.mass_earth.toFixed(4);
      }

      if (isNaN(filtered_planet.radius_jupiter)) {
        filtered_planet.radius_jupiter = '--';
        filtered_planet.radius_earth = '--';
      }
      else {
        filtered_planet.radius_jupiter = filtered_planet.radius_jupiter.toFixed(4);
        filtered_planet.radius_earth = filtered_planet.radius_earth.toFixed(4);
      }

      sorted.push(filtered_planet);
    }
  }

  sorted.sort(function (a, b) {
    return b.date - a.date;
  });

  return sorted;
}

/**
 * Holy crappola! Copied from the API? Cheeses, I should never have agreed to
 * Node.JS and instead used something I was passingly familiar with.
 *
 * I guess node doesn't have this. Seems kind of unlikely, but...
 *
 * @param string line
 *   A string with some commas in it.
 *
 * @return
 *   A list of some strings.
 */
function csv_columns(line) {
  var cols = [];
  var prev_c = null;
  var col_buffer = '';
  var in_string = false;

  var chars = line.split('');
  for (var ii in chars) {
    var c = chars[ii];

    if (c == ',' && !in_string) {
      cols.push(col_buffer);
      col_buffer = '';
      continue;
    }

    if (c == '"' && prev_c != '\\') {
      in_string = !in_string;
    }

    if (!in_string || (in_string && c != '\\')) {
      col_buffer += c;
    }

    prev_c = c;
  }

  cols.push(col_buffer);

  return cols;
}
