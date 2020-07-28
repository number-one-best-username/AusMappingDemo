import 'ol/ol.css';
import Map from 'ol/Map';
import View from 'ol/View';
import {createEmpty, getWidth, getHeight, extend} from 'ol/extent';
import GeoJSON from 'ol/format/GeoJSON';
import {Vector as VectorLayer, Tile as TileLayer} from 'ol/layer';
import {TileArcGISRest, OSM, Cluster} from 'ol/source';
import VectorSource from 'ol/source/Vector';
import {Circle as CircleStyle, Fill, RegularShape, Stroke, Style, Text} from 'ol/style';

var currentResolution, maxFeatureCount;
var sourceFilter = null;
var initDate = Date.now();

function getUniques(fs) {
    return fs.filter(function(v, i, self) {
	return self.indexOf(v) === i;
    });
}

var calculateClusterInfo = function(resolution) {
    maxFeatureCount = 0;
    var features = vector.getSource().getFeatures();
    var feature, area, population;
    for (var i = features.length - 1; i >= 0; --i) {
	feature = features[i];
	area = 0;
	population = 0;
	var originalFeatures = feature.get('features');
	var extent = createEmpty();
	var sa2 = [], sa3 = [], sa4 = [], sa5 = [], sa6 = [];
	var j = (void 0), jj = (void 0);
	for (j = 0, jj = originalFeatures.length; j < jj; ++j) {
	    extend(extent, originalFeatures[j].getGeometry().getExtent());
	    sa2.push(originalFeatures[j].get('sa2'));
	    sa3.push(originalFeatures[j].get('sa3'));
	    sa4.push(originalFeatures[j].get('sa4'));
	    sa5.push(originalFeatures[j].get('sa5'));
	    sa6.push(originalFeatures[j].get('sa6'));
	    sa6.push(originalFeatures[j].get('sa6'));
	    population += originalFeatures[j].get('population');
	    area += originalFeatures[j].get('area');
	}

	var target;
	if(sa2.length <= 4) {
	    target = {
		"level": 'sa2',
		"locations": getUniques(sa2)
	    };
	} else {
	    sa3 = getUniques(sa3);
	    if(sa3.length <= 4) {
		target = {
		    "level": 'sa3',
		    "locations": getUniques(sa3)
		};
	    } else {
		sa4 = getUniques(sa4);
		if(sa4.length <= 4) {
		    target = {
			"level": 'sa4',
			"locations": getUniques(sa4)
		    };
		} else {
		    sa5 = getUniques(sa5);
		    if(sa5.length <= 2) {
			target = {
			    "level": 'sa5',
			    "locations": getUniques(sa5)
			};
		    } else {
			target = {
			    "level": 'sa6',
			    "locations": getUniques(sa6)
			};
		    }
		}
	    }
	}

	feature.set('target', target);
	var popDense = population/area;
	maxFeatureCount = Math.max(maxFeatureCount, popDense);
	feature.set('size', popDense);
    }
};

function styleFunction(feature, resolution) {
    if (resolution != currentResolution) {
	calculateClusterInfo(resolution);
	currentResolution = resolution;
    }
    var size = feature.get('size')/maxFeatureCount;
    return new Style({
	image: new CircleStyle({
            radius: 2 + 23*Math.exp(size),
            fill: new Fill({
		color: [55, 225, 175, Math.min(0.75, 0.25 + 0.75 * size)]
            })
	}),
	text: new Text({
            text: feature.get('name'),
            fill: new Fill({
		color: '#fff'
	    }),
            stroke: new Stroke({
		color: 'rgba(0, 0, 0, 0.6)',
		width: 3
	    })
	})
    });
}

var vector = new VectorLayer({
    source: new Cluster({
	source: new VectorSource({
	    format: new GeoJSON(),
	    url: './geo.json?' + initDate
	}),
	geometryFunction: function(feature) {
	    return feature.getGeometry();
	},
	distance: 75
    }),
    style: styleFunction
});

var map = new Map({
    layers: [
	new TileLayer({
	    source: new OSM()
	}),
	// new TileLayer({
	//     source: new TileArcGISRest({
	// 	url: "https://geo.abs.gov.au/arcgis/rest/services/ASGS2019/LGA/MapServer"
	//     })
	// }),
	vector,
    ],
    target: 'map',
    view: new View({
	projection: 'EPSG:4326',
	center: [140, -30],
	zoom: 5
    })
});

window.onload = function () {
    // Create an observer instance linked to the callback function
    const observer = new MutationObserver(function(mutationsList, observer) {
	for(let mutation of mutationsList) {
	    if (mutation.type === 'attributes') {
		sourceFilter = JSON.parse(mutation.target.getAttribute('data-json'));
		currentResolution = null;
		vector.getSource().refresh();
            }
	}
    });

    // Start observing the target node for configured mutations
    observer.observe(document.getElementById('filtered_json'), {attributes: true});

    // Later, you can stop observing
    //observer.disconnect();
};

var app = Elm.Main.init({
    node: document.getElementById('elm-controls'),
    flags: initDate
});

var selected = null;

map.on('pointermove', function(e) {
    map.forEachFeatureAtPixel(e.pixel, function(f) {
	if(f.get('target') != selected) {
	    selected = f.get('target');
	    app.ports.subTarget.send(JSON.stringify(selected));
	}
    });
});
