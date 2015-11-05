var weather = c3.generate({
  bindto: '#weather-plot',
  padding: {
        top: 40,
        right: 100,
        bottom: 40,
        left: 100,
    },
  data: {
	url: 'data/trip_weather_reduced.csv',
  names: {
    annual: 'Annual Members',
    shortt: 'Day-pass holders',
    precip: 'Precipitation Index',
    temper: 'Mean Temperature'
  },
	axes: {
		annual: 'y',
		shortt: 'y',
		precip: 'y2',
		temper: 'y2'
	    },
    x: 'date',
    xFormat: '%Y-%m-%d',
    type: 'bar',
	types: {
		temper: 'line',
		precip: 'area-spline',
		annual: 'bar',
		shortt: 'bar'
	},
	colors: {
		temper: '#8EDD65',
		precip: '#68D2DF',
		annual: '#003B49',
		shortt: '#058E9E'
	}
  },
  bar: {
	width: {ratio:10}
      },
  regions: [
       {start: '2014-10-13', end: '2014-11-01'},
       {start: '2014-12-01', end: '2015-01-01'},
       {start: '2015-02-01', end: '2015-03-01'},
       {start: '2015-04-01', end: '2015-05-01'},
       {start: '2015-06-01', end: '2015-07-01'},
       {start: '2015-08-01', end: '2015-09-01'},
       {start: '2015-10-01', end: '2015-10-12'},
   ],
  axis: {
	x: {
		type: 'timeseries',
		tick: {
			annual: 24,
			format: '%a, %d-%b-%Y'
			},

		},
	y2: {
		show: true
	}
	},
  zoom: {
   enabled: true
  }
 });

var donut = c3.generate({
	bindto: '#donut',
    data: {
        columns: [
            ['Annual member', 87360],
            ['Short-term pass holder', 55486],
        ],
        colors: {
			'Annual member': '#003B49',
			'Short-term pass holder': '#058E9E'
		},
        type : 'donut'
    },
    donut: {
        title: "142,846 rides"
    }
});

var donut2 = c3.generate({
	bindto: '#donut2',
    data: {
        columns: [
            ['Annual member', 14722.48],
            ['Short-term pass holder', 33326.84],
        ],
        colors: {
			'Annual member': '#003B49',
			'Short-term pass holder': '#058E9E'
		},
        type : 'donut'
    },
    donut: {
        title: "Over 48,000 hours"
    }
});

var bar = c3.generate({
  bindto: '#bar',
    data: {
        columns: [
            ['annual', 10],
            ['shortterm', 36]
        ],
        type: 'bar',
        colors: {
          annual: '#003B49',
          shortterm: '#058E9E'
		     },
    },
    bar: {
        width: {
            ratio: 0.5 // this makes bar width 50% of length between ticks
        }
        // or
        //width: 100 // this makes bar width 100px
    },
    axis: {
      rotated: true
    }
});

var region_data=[
  ['Belltown','Belltown',2200],
  ['Belltown','Downtown',3587],
  ['Belltown','Central District',5],
  ['Belltown','Capitol Hill',728],
  ['Belltown','South Lake Union',842],
  ['Belltown','East Lake',627],
  ['Belltown','First Hill',49],
  ['Belltown','International District',180],
  ['Belltown','Pioneer Square',982],
  ['Belltown','South Lake Union',4809],
  ['Belltown','University District',111],
  ['Belltown','UW',50],
  ['Belltown','Water Front',1503],
  ['Downtown','Belltown',2974],
  ['Downtown','Downtown',3195],
  ['Downtown','Central District',27],
  ['Downtown','Capitol Hill',1341],
  ['Downtown','South Lake Union',184],
  ['Downtown','East Lake',305],
  ['Downtown','First Hill',159],
  ['Downtown','International District',608],
  ['Downtown','Pioneer Square',2174],
  ['Downtown','South Lake Union',4175],
  ['Downtown','University District',75],
  ['Downtown','UW',31],
  ['Downtown','Water Front',1544],
  ['Central District','Belltown',8],
  ['Central District','Downtown',73],
  ['Central District','Central District',13],
  ['Central District','Capitol Hill',116],
  ['Central District','South Lake Union',2],
  ['Central District','East Lake',1],
  ['Central District','First Hill',36],
  ['Central District','International District',57],
  ['Central District','Pioneer Square',82],
  ['Central District','South Lake Union',17],
  ['Central District','University District',3],
  ['Central District','Water Front',32],
  ['Capitol Hill','Belltown',2150],
  ['Capitol Hill','Downtown',6493],
  ['Capitol Hill','Central District',236],
  ['Capitol Hill','Capitol Hill',11698],
  ['Capitol Hill','South Lake Union',411],
  ['Capitol Hill','East Lake',1133],
  ['Capitol Hill','First Hill',1284],
  ['Capitol Hill','International District',677],
  ['Capitol Hill','Pioneer Square',1502],
  ['Capitol Hill','South Lake Union',8105],
  ['Capitol Hill','University District',594],
  ['Capitol Hill','UW',388],
  ['Capitol Hill','Water Front',355],
  ['South Lake Union','Belltown',524],
  ['South Lake Union','Downtown',165],
  ['South Lake Union','Central District',3],
  ['South Lake Union','Capitol Hill',112],
  ['South Lake Union','South Lake Union',316],
  ['South Lake Union','East Lake',282],
  ['South Lake Union','First Hill',12],
  ['South Lake Union','International District',4],
  ['South Lake Union','Pioneer Square',79],
  ['South Lake Union','South Lake Union',1212],
  ['South Lake Union','University District',412],
  ['South Lake Union','UW',235],
  ['South Lake Union','Water Front',35],
  ['East Lake','Belltown',559],
  ['East Lake','Downtown',244],
  ['East Lake','Central District',1],
  ['East Lake','Capitol Hill',199],
  ['East Lake','South Lake Union',284],
  ['East Lake','East Lake',1515],
  ['East Lake','First Hill',22],
  ['East Lake','International District',7],
  ['East Lake','Pioneer Square',82],
  ['East Lake','South Lake Union',2473],
  ['East Lake','University District',601],
  ['East Lake','UW',472],
  ['East Lake','Water Front',71],
  ['First Hill','Belltown',177],
  ['First Hill','Downtown',708],
  ['First Hill','Central District',48],
  ['First Hill','Capitol Hill',1345],
  ['First Hill','South Lake Union',30],
  ['First Hill','East Lake',101],
  ['First Hill','First Hill',231],
  ['First Hill','International District',225],
  ['First Hill','Pioneer Square',389],
  ['First Hill','South Lake Union',568],
  ['First Hill','University District',18],
  ['First Hill','UW',10],
  ['First Hill','Water Front',38],
  ['International District','Belltown',100],
  ['International District','Downtown',482],
  ['International District','Central District',18],
  ['International District','Capitol Hill',214],
  ['International District','South Lake Union',6],
  ['International District','East Lake',5],
  ['International District','First Hill',80],
  ['International District','International District',137],
  ['International District','Pioneer Square',140],
  ['International District','South Lake Union',132],
  ['International District','University District',4],
  ['International District','UW',3],
  ['International District','Water Front',143],
  ['Pioneer Square','Belltown',475],
  ['Pioneer Square','Downtown',1372],
  ['Pioneer Square','Central District',21],
  ['Pioneer Square','Capitol Hill',288],
  ['Pioneer Square','South Lake Union',64],
  ['Pioneer Square','East Lake',53],
  ['Pioneer Square','First Hill',99],
  ['Pioneer Square','International District',156],
  ['Pioneer Square','Pioneer Square',465],
  ['Pioneer Square','South Lake Union',764],
  ['Pioneer Square','University District',5],
  ['Pioneer Square','Water Front',960],
  ['South Lake Union','Belltown',4365],
  ['South Lake Union','Downtown',4601],
  ['South Lake Union','Central District',13],
  ['South Lake Union','Capitol Hill',1781],
  ['South Lake Union','South Lake Union',1454],
  ['South Lake Union','East Lake',2703],
  ['South Lake Union','First Hill',141],
  ['South Lake Union','International District',179],
  ['South Lake Union','Pioneer Square',1038],
  ['South Lake Union','South Lake Union',14944],
  ['South Lake Union','University District',513],
  ['South Lake Union','UW',324],
  ['South Lake Union','Water Front',1340],
  ['University District','Belltown',106],
  ['University District','Downtown',69],
  ['University District','Central District',5],
  ['University District','Capitol Hill',192],
  ['University District','South Lake Union',361],
  ['University District','East Lake',575],
  ['University District','First Hill',7],
  ['University District','International District',1],
  ['University District','Pioneer Square',7],
  ['University District','South Lake Union',475],
  ['University District','University District',1984],
  ['University District','UW',1784],
  ['University District','Water Front',21],
  ['UW','Belltown',29],
  ['UW','Downtown',77],
  ['UW','Capitol Hill',195],
  ['UW','South Lake Union',212],
  ['UW','East Lake',601],
  ['UW','First Hill',2],
  ['UW','International District',1],
  ['UW','Pioneer Square',6],
  ['UW','South Lake Union',309],
  ['UW','University District',1731],
  ['UW','UW',2828],
  ['UW','Water Front',29],
  ['Water Front','Belltown',833],
  ['Water Front','Downtown',1339],
  ['Water Front','Central District',16],
  ['Water Front','Capitol Hill',47],
  ['Water Front','South Lake Union',32],
  ['Water Front','East Lake',27],
  ['Water Front','First Hill',17],
  ['Water Front','International District',93],
  ['Water Front','Pioneer Square',1125],
  ['Water Front','South Lake Union',763],
  ['Water Front','University District',28],
  ['Water Front','UW',23],
  ['Water Front','Water Front',5560]
];

var width = 2000, height = 1000, margin ={b:0, t:40, l:170, r:50};

var svg = d3.select("#bipartite")
	.append("svg").attr('width',width).attr('height',(height+margin.b+margin.t))
	.append("g").attr("transform","translate("+ margin.l+","+margin.t+")");

var data = [
	{data:bP.partData(region_data,2), id:'Trips', header:["From","To", "Number of Trips"]}
];

bP.draw(data, svg);

svg.selectAll("text.barlabel").style("margin-left", "50%");
