var csv = require('csv');
var fs = require('fs');

var outFile = fs.openSync('js/array.js', 'w+');

var outdata = [];

csv().fromPath(__dirname+'/exoplanets.csv', 
		{   delimiter:','
		, 	rtrim:true
		,	columns:true
		})
	.on('data', function(data){
		//console.log('' + data.NAME);
		
		var name = data.NAME;
		var ra = parseFloat(data.RA);
		var dec = parseFloat(data.DEC);
		var dist = parseFloat(data.DISTANCE);

		var newoutput = {
			name: name,
			ra: ra,
			dec: dec,
			dist: dist,
			mass: parseFloat(data.MSINI),
			temp: parseFloat(data.TEFF)
		}
		
		if( name && ra && dec && dist)
			outdata.push(newoutput);
		
	})

	.on('end', function(count){
		console.log(' read these number of lines: ' + count);
		console.log(' wrote this number of lines: ' + outdata.length);
		
		fs.writeSync(outFile, 'exo_array = ');
		fs.writeSync(outFile, JSON.stringify(outdata));
		fs.closeSync(outFile);
	})
	;
	
