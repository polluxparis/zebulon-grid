/* global module, require, React */

'use strict';

const React = require('react'),
    axe = require('../orb.axe'),
    PivotButton = require('./orb.react.PivotButton.jsx'),
    DropTarget = require('./orb.react.DropTarget.jsx');

module.exports = React.createClass({
  render: function() {
    var self = this;
    var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    var columnButtons = config.columnFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.COLUMNS}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    return  <DropTarget buttons={columnButtons} axetype={axe.Type.COLUMNS}>
            </DropTarget>;
  }
});