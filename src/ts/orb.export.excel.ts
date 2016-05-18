// /**
//  * @fileOverview Pivot Grid export to Excel
//  * @author Najmeddine Nouri <najmno@gmail.com>
//  */
//
//  'use strict';
//
//  /* global module, require */
//  /*jshint eqnull: true*/
//
// import * as utils from './orb.utils';
// import {PGridWidgetStore} from './orb.ui.pgridwidgetstore';
// import {HeaderType} from './orb.ui.header';
// import {ThemeManager} from './orb.themes';
//
// const uriHeader = 'data:application/vnd.ms-excel;base64,';
// const docHeader = '<html xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns="http://www.w3.org/TR/REC-html40">' +
//  	'<head>' +
//  	'<meta http-equiv=Content-Type content="text/html; charset=UTF-8">' +
//  	'<!--[if gte mso 9]><xml>' +
//  	' <x:ExcelWorkbook>' +
//  	'  <x:ExcelWorksheets>' +
//  	'   <x:ExcelWorksheet>' +
//  	'    <x:Name>###sheetname###</x:Name>' +
//  	'    <x:WorksheetOptions>' +
//  	'     <x:ProtectContents>False</x:ProtectContents>' +
//  	'     <x:ProtectObjects>False</x:ProtectObjects>' +
//  	'     <x:ProtectScenarios>False</x:ProtectScenarios>' +
//  	'    </x:WorksheetOptions>' +
//  	'   </x:ExcelWorksheet>' +
//  	'  </x:ExcelWorksheets>' +
//  	'  <x:ProtectStructure>False</x:ProtectStructure>' +
//  	'  <x:ProtectWindows>False</x:ProtectWindows>' +
//  	' </x:ExcelWorkbook>' +
//  	'</xml><![endif]-->' +
//  	'</head>' +
//  	'<body>';
// const docFooter = '</body></html>';
//
// /**
//  * Creates a new instance of rows ui properties.
//  * @class
//  * @memberOf orb.ui
//  * @param  {orb.axe} rowsAxe - axe containing all rows dimensions.
//  */
// export default (pgridwidgetstore: PGridWidgetStore) => {
//
//  	var config = pgridwidgetstore.pgrid.config;
//
// 	const themeManager = config.theme;
//  	var currTheme = themeManager.current();
//  	currTheme = currTheme === 'bootstrap' ? 'white' : currTheme;
//  	var override = currTheme === 'white';
//
//  	var buttonTextColor = override ? 'black' : 'white';
//  	var themeColor = themeManager.themes[currTheme];
//  	var themeFadeout = themeManager.utils.fadeoutColor(themeColor, 0.1);
//
//  	var buttonStyle = 'style="font-weight: bold; color: ' + buttonTextColor + '; background-color: ' + themeColor + ';" bgcolor="' + themeColor + '"';
//  	var headerStyle = 'style="background-color: ' + themeFadeout + ';" bgcolor="' + themeFadeout + '"';
//
//  	function createButtonCell(caption) {
//  		return '<td ' + buttonStyle + '><font color="' + buttonTextColor + '">' + caption + '</font></td>';
//  	}
//
//  	function createButtons(buttons, cellsCountBefore, cellsCountAfter, prefix?) {
//  		var i;
//  		var str = prefix || '<tr>';
//  		for(i = 0; i < cellsCountBefore; i++) {
//  			str += '<td></td>';
//  		}
//
//  		str += buttons.reduce(function(tr, field) {
//  			return (tr += createButtonCell(field.caption));
//  		}, '');
//
//  		for(i = 0; i < cellsCountAfter; i++) {
//  			str += '<td></td>';
//  		}
//  		return str + '</tr>';
//  	}
//
//  	var cellsHorizontalCount = Math.max(config.dataFields.length + 1, pgridwidgetstore.layout.pivotTable.width);
//
//  	var dataFields = createButtons(config.dataFields,
//  		0,
//  		cellsHorizontalCount - config.dataFields.length,
//  		'<tr><td><font color="#ccc">Data</font></td>'
//  	);
//
//  	var sep = '<tr><td style="height: 22px;" colspan="' + cellsHorizontalCount + '"></td></tr>';
//
//  	var columnFields = createButtons(config.columnFields,
//  		pgridwidgetstore.layout.rowHeaders.width,
//  		cellsHorizontalCount - (pgridwidgetstore.layout.rowHeaders.width + config.columnFields.length)
//  	);
//
//  	var columnHeaders = (function() {
//  		var str = '';
//  		var j;
//  		for(var i = 0; i < pgridwidgetstore.columns.headers.length; i++) {
//  			var currRow = pgridwidgetstore.columns.headers[i];
//  			var rowStr = '<tr>';
//  			if(i < pgridwidgetstore.columns.headers.length - 1) {
//  				for(j = 0; j < pgridwidgetstore.layout.rowHeaders.width; j++) {
//  					rowStr += '<td></td>';
//  				}
//  			} else {
//  				rowStr += config.rowFields.reduce(function(tr, field) {
//  					return (tr += createButtonCell(field.caption));
//  				}, '');
//  			}
//
//  			rowStr += currRow.reduce(function(tr, header) {
//  				var value = header.type === HeaderType.DATA_HEADER ? header.value.caption : header.value;
//  				return (tr += '<td ' + headerStyle + ' colspan="' + header.hspan(true) + '" rowspan="' + header.vspan(true) + '">' + value + '</td>');
//  			}, '');
//  			str += rowStr + '</tr>';
//  		}
//  		return str;
//  	}());
//
//  	var rowHeadersAndDataCells = (function() {
//  		var str = '';
//  		var j;
//  		for(var i = 0; i < pgridwidgetstore.rows.headers.length; i++) {
//  			var currRow = pgridwidgetstore.rows.headers[i];
//  			var rowStr = '<tr>';
//  			rowStr += currRow.reduce(function(tr, header) {
//  				return (tr += '<td ' + headerStyle + ' colspan="' + header.hspan(true) + '" rowspan="' + header.vspan(true) + '">' + header.value + '</td>');
//  			}, '');
//  			var dataRow = pgridwidgetstore.dataRows[i];
//  			rowStr += dataRow.reduce(function(tr, dataCell, index) {
//  				var formatFunc = config.dataFields[index = index % config.dataFields.length].formatFunc;
//  				var value = dataCell.value == null ? '' : formatFunc ? formatFunc()(dataCell.value) : dataCell.value;
//  				return (tr += '<td>' + value + '</td>');
//  			}, '');
//  			str += rowStr + '</tr>';
//  		}
//  		return str;
//  	}());
//
//  	function toBase64(str) {
//  		const btoaFunc = utils.btoa
//  		return btoaFunc(decodeURIComponent(encodeURIComponent(str)));
//  	}
//
//  	return uriHeader +
//  		toBase64(docHeader +
//  				'<table>' + dataFields + sep + columnFields + columnHeaders + rowHeadersAndDataCells + '</table>' +
//  				docFooter);
//  };
